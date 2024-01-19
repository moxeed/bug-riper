Morteza Bahjat 810198363  
Kianosh Arian  810198345

*Using **intel/hpckit** Docker Image*

### Part #1: Finding Hostspots

```
> icpx src/CA#05_code.cpp
> vtune -c hotspots ./a.out
```

```
# solutions 2279184 time: 21.000000

Elapsed Time: 21.574s
    CPU Time: 21.548s
        Effective Time: 21.548s
        Spin Time: 0s
        Overhead Time: 0s
    Total Thread Count: 1
    Paused Time: 0s

Top Hotspots
Function  Module  CPU Time  % of CPU Time(%)
--------  ------  --------  ----------------
put       a.out    21.548s            100.0%
Collection and Platform Info
    Application Command Line: ./a.out
    Operating System: 5.15.133.1-microsoft-standard-WSL2 DISTRIB_ID=Ubuntu DISTRIB_RELEASE=22.04 DISTRIB_CODENAME=jammy DISTRIB_DESCRIPTION="Ubuntu 22.04.3 LTS"
    Computer Name: 7d1d8e598dda
    Result Size: 6.9 MB
    Collection start time: 13:58:20 19/01/2024 UTC
    Collection stop time: 13:58:41 19/01/2024 UTC
    Collector Type: User-mode sampling and tracing
    CPU
        Name: Unknown
        Frequency: 2.688 GHz
        Logical CPU Count: 4
        Cache Allocation Technology
            Level 2 capability: not detected
            Level 3 capability: not detected
```


### Part #2: OpenMP

```diff
int put(int Queens[], int row, int column)
{
    int i;

    for (i = 0; i < row; i++)
    {
        if (Queens[i] == column || abs(Queens[i] - column) == (row - i))
            return -1;
    }
    Queens[row] = column;
    if (row == N - 1)
    {
+       #pragma omp atomic 
        solutions++;
    }
    else
    {
        for (i = 0; i < N; i++)
        { // increment row
            put(Queens, row + 1, i);
        }
    }

    return 0;
}
-void solve(Queens)
+void solve()
{
    int i;
+   #pragma omp parallel num_threads(N)
    {
+       #pragma omp for
        for (i = 0; i < N; i++)
        {
-            put(Queens, 0, i);
+            put(new int[N], 0, i);
        }
    }
}

int main()
{
-   int Queens[N];
    time_t t0 = 0, tf = 0, t0s = 0, tfs = 0;

    t0 = time(NULL);
-   solve(Queens); 
+   solve();
    tf = time(NULL);

    printf("# solutions %d time: %f \n", solutions, difftime(tf, t0));

    return 0;
}
```
output:
```
> ./omp.out
# solutions 2279184 time: 6.000000 
```


### Part #3: Inspector Analyzes

mi1   Detect Leaks   
mi2   Detect Memory Problems  
mi3   Locate Memory Problems
```
> inspxe-cl -collect mi3 module-filter-mode=include -module-filter=/app/omp.out ./omp.out
# solutions 2279184 time: 43.000000 
  
2 new problem(s) found
    1 Invalid memory access problem(s) detected 
    1 Memory leak problem(s) detected

> inspxe-cl -report observations
omp.out!0x14e5: Error X1: P1: Memory leak: Allocation site: Function _Z5solvev.extracted: Module /app/omp.out   
libc.so.6!0x7fa4ecfd0000: Error X2: P2: Invalid memory access: Read: Function [Unknown]: Module /usr/lib/x86_64-linux-gnu/libc.so.6
```
Fix: 
```diff
void solve()
{
    int i;
    #pragma omp parallel num_threads(N)
    {
        #pragma omp for
        for (i = 0; i < N; i++)
        {
-           put(new int[N], 0, i);
+           int queens[N]={0};
+           put(queens, 0, i);
        }
    }
}
```

ti1   Detect Deadlocks   
ti2   Detect Deadlocks and Data Races  
ti3   Locate Deadlocks and Data Races

```
> inspxe-cl -collect ti3 -module-filter-mode=include -module-filter=/app/omp.out ./omp.out
# solutions 2279184 time: 493.000000 
  
0 new problem(s) found
```
