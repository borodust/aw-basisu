param (
    [parameter(Mandatory=$false)]
    [int] $BuildThreadCount=0,
    [parameter(Mandatory=$false)]
    [String] $BuildType="MinSizeRel"
)

### CONFIGURATION START ###
$ErrorActionPreference = "Stop"

if ( $BuildThreadCount -eq 0 ) {
    $ComputerSystem = Get-CimInstance -class Win32_ComputerSystem
    $BuildThreadCount = $ComputerSystem.NumberOfLogicalProcessors - 1
    if ( $BuildThreadCount -le 0) {
        $BuildThreadCount = 1
    }
}

Write-Output "Build thread count: $BuildThreadCount"
Write-Output "Build type: $BuildType"

$WorkDir = $PSScriptRoot
$BuildDir = "$WorkDir/build/desktop/"

### CONFIGURATION END ###

md $BuildDir -Force | Out-Null
pushd $BuildDir

cmake -G "Visual Studio 16 2019" -A x64 -Thost=x64 `
      -DCMAKE_BUILD_TYPE=$BuildType `
      -DCMAKE_C_FLAGS="-DBASISD_SUPPORT_KTX2_ZSTD=1 -DBASISU_SUPPORT_SSE=1 /arch:AVX" `
      -DCMAKE_CXX_FLAGS="-DBASISD_SUPPORT_KTX2_ZSTD=1 -DBASISU_SUPPORT_SSE=1 /arch:AVX" `
  "$WorkDir"


cmake --build "$BuildDir" --config $BuildType --parallel $BuildThreadCount

cp $BuildDir/$BuildType/basisu.clawed.dll $BuildDir/

popd
