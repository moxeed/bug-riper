param(
  [string] $file,
  [string] $hieFile,
  [string] $function
)

Remove-Item *.out | Out-Null
New-Item ./cummulative.run.out | Out-Null

Write-Output "G2 Started"
$tests = (G2 --n 800 --max-outputs 32  $file $function)
Write-Output "G2 Finished"

Copy-Item .\src\MainTemple.hs .\src\Main.hs
Copy-Item .\src\SuitTemple.hs .\src\TestSuite.hs

foreach ($test in $tests) {
    $testId = ($test -replace "=.*", "")
    Add-Content .\src\Main.hs "run ""$testId"" = show $ $testId" 
}

$best = 0
foreach ($test in $tests) {
    $testId = ($test -replace "=.*", "")

    Write-Output "Checking: $testId"

    cabal run bug-riper -- $testId | Out-Null
    cabal.exe run analyze -- --ju $hieFile ./run.out 
    $covered = cabal.exe run analyze -- --ju $hieFile ./run.out |
        Select-String -Pattern "NotCovered:" -NotMatch

    if ($covered.length -gt $best)
    {
        if ($best -eq 0)
        {
            Add-Content .\src\TestSuite.hs "    $testId"
        }
        else {
            Add-Content .\src\TestSuite.hs "   ,$testId"
        }
        
        $best = $covered.length
        Write-Output $test
    }
}

Add-Content .\src\TestSuite.hs "    ]"
Copy-Item .\src\TestSuite.hs .\src\Main.hs
Remove-Item .\src\TestSuite.hs