# Specific setup in my environment for Docker

docker_func=$HOME/.dockerfunc
[ -f $docker_func ] && source $docker_func

# Colima
# Set the default Docker daemon socket to Colima's location. I use Colima
# instead of the Docker Desktop application.
# https://github.com/nektos/act/issues/1051#issuecomment-1070599087
export DOCKER_HOST="unix://$HOME/.colima/docker.sock"
