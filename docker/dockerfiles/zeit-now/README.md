# docker-zeit-now

A Docker image to run [Zeit's Now][zn] as an executable but running in a container:

```bash
docker run --rm -it \
  -v $PWD:/now \
  andrewsardone/docker-zeit-now
# Output of running `now`
```

## Why would you use this?

If you want to [`now`][zn] to deploy a simple service or static site, but
don't want to install Node.js on your host. Instead, run `now` within a
container where the credentials (`~/.now`) live on the host.

[zn]: https://zeit.co/now

## Building

Since credentials are built into the image (pro-tip: don't deploy this on a
public registry), you'll need to create the image locally.

1. Place your `now` credentials within the `now-creds` directory
    - `mkdir -p now-creds && cp -R ~/.now/. now-creds/.`
2. Build the image with `./build.sh`
