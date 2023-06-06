
param(
  [string] $file,
  [string] $hieFile,
  [string] $function
)

Remove-Item *.out | Out-Null
New-Item ./cummulative.run.out | Out-Null

Write-Output "G2 Started"
$tests = (G2 --max-outputs 100 $file $function)
Write-Output "G2 Finished"
$totalBest = 0

Copy-Item .\src\MainTemple.hs .\src\Main.hs | Out-Null
foreach ($test in $tests) {
    $testId = ($test -replace "=.*", "")
    Add-Content .\src\Main.hs "run ""$testId"" = show $ $testId" 
}

while (1) {

    $best = 0
    $bestTest = ""
    foreach ($test in $tests) {
        Copy-Item ./cummulative.run.out ./run.out | Out-Null

        $testId = ($test -replace "=.*", "")
        cabal run bug-riper -- $testId | Out-Null
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