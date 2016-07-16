# stack-updater
Update package dependencies that are using Github in `stack.yaml`

This is still work in progress but usable (not perfect but usable)

## Installing
Build from source for now

## Usage

Update specific repo
```
$ stack-updater update <reponame>
```

or choose repo interactively
```
$ stack-updater update
```

`<reponame>` must be in full form `owner/repo` like `shulhi/stack-updater`


## Limitations
- Only works for git repo hosted on Github
- No support for mercurial
