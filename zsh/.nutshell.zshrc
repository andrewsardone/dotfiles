# REDACTED
# Include Nutshell specific shell configuration

releaseTickets() {
  echo "https://github.com/nutshellcrm/nutshell/compare/$1...$2"
  echo ''
  git log $1..$2 | \
    egrep -io 'NUT-\d+' | \
    tr 'a-z' 'A-Z' | \
    uniq | \
    sort | \
    sed -e 's/^/https:\/\/nutshell.atlassian.net\/browse\//'
}

openReleaseTickets() {
  releaseTickets $1 $2 | xargs open
}

alias aws-nut-ec2="aws ec2 describe-instances --filters "Name=instance-state-name,Values=running" --query 'Reservations[*].Instances[*].[Tags[?Key==\`Name\`] | [0].Value,InstanceId,InstanceType,PrivateIpAddress]' --output table"
