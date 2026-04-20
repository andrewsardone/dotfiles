{ ... }:
{
  programs.git = {
    enable = true;

    includes = [
      { condition = "gitdir:~/source/code.amazon.com/"; path = "~/.gitconfig.amazon"; }
      { condition = "gitdir:~/workplace/";              path = "~/.gitconfig.amazon"; }
      { condition = "gitdir:/workplace/";               path = "~/.gitconfig.amazon"; }
      { condition = "gitdir:/Volumes/workplace/";       path = "~/.gitconfig.amazon"; }
      { condition = "gitdir:~/brazil-workspaces/";      path = "~/.gitconfig.amazon"; }
    ];

    signing.format = null;

    settings = {
      user.name  = "Andrew Sardone";
      user.email = "andrew@andrewsardone.com";

      alias = {
        co       = "checkout";
        shortsha = "rev-parse --short HEAD";
        st       = "status --short --branch";
        ts       = "st";
        lg       = "log --graph --pretty=format:'%Cred%h%Creset %C(cyan)%an%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative";
        recent   = "! git reflog | grep 'checkout: moving from' | head -n 50 | egrep -oh ' \\S+$' | awk '!x[$0]++'";
      };

      color.ui = "auto";

      core = {
        excludesfile   = "~/.gitexcludes";
        attributesfile = "~/.gitattributes";
      };

      "diff \"localizablestrings\"" = {
        textconv = "iconv -f utf-16 -t utf-8";
      };

      hub.http-clone = true;

      github.user = "andrewsardone";

      merge.defaultToUpstream = true;

      commit.verbose = true;

      init.defaultBranch = "main";

      "credential \"https://github.com\"" = {
        helper = "!/opt/homebrew/bin/gh auth git-credential";
      };
      "credential \"https://gist.github.com\"" = {
        helper = "!/opt/homebrew/bin/gh auth git-credential";
      };
    };
  };
}
