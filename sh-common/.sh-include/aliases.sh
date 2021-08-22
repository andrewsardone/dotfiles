if command -v ncdu >/dev/null 2>&1; then
  alias ncdu="ncdu --color dark"
fi

if command -v bat >/dev/null 2>&1; then
  alias cat="bat --paging=never"
fi
