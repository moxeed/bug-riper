
param(
  [string] $file,
  [string] $hieFile,
  [string] $function
)

Remove-Item *.out | Out-Null
New-Item ./cummulative.run.out | Out-Null

$tests = (.\G2.exe $file $function)
$totalBest = 0

while (1) {

    $best = 0
    $bestTest = ""
    foreach ($test in $tests) {
        (Get-Content .\src\MainTemple.hs) -replace "%test%", ($test -replace "=.*", "") | Set-Content .\src\Main.hs
        Copy-Item ./cummulative.run.out ./run.out | Out-Null

        cabal run bug-riper | Out-Null
        $covered = cabal.exe run analyze -- --ju $hieFile ./run.out |
            Select-String -Pattern "NotCovered:" -NotMatch

        if ($covered.length -gt $best)
        {
            $best = $covered.length
            $bestTest = $test
            Copy-Item ./run.out ./best.run.out
        }
    }

    if ($totalBest -ge $best){
        break
    }
        
    $totalBest = $best
    Copy-Item ./best.run.out ./cummulative.run.out
    Write-Output $bestTest
}