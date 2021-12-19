set -eu

thisDir=$(dirname "$0")
baseDir=$thisDir/../

$baseDir/happy-path/lock-tx.sh 0 10 300
$baseDir/wait/until-next-block.sh
sleep 20

echo Early Close Fails
detected=false

"$baseDir/failure-cases/unlock-first-bad-datum-tx.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/wait/until-next-block.sh
$baseDir/happy-path/unlock-first-tx.sh
$baseDir/wait/until-next-block.sh
sleep 300
$baseDir/happy-path/unlock-tx.sh
$baseDir/wait/until-next-block.sh
