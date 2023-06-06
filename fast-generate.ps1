param(
  [string] $file,
  [string] $hieFile,
  [string] $function
)

Remove-Item *.out | Out-Null
New-Item ./cummulative.run.out | Out-Null

Write-Output "G2 Started"
$tests = (G2 --max-outputs 10 $file $function)
Write-Output "G2 Finished"

foreach ($test in $tests) {
    $testId = ($test -replace "=.*", "")
    Add-Content .\src\Main.hs "run ""$testId"" = show $ $testId" 
}

$best = 0
foreach ($test in $tests) {
    $testId = ($test -replace "=.*", "")
    cabal run bug-riper -- $testId | Out-Null
    $covered = cabal.exe run analyze -- --ju $hieFile ./run.out |
        Select-String -Pattern "NotCovered:" -NotMatch

    if ($covered.length -gt $best)
    {
        $best = $covered.length
        Write-Output $test
    }
}