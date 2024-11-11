#!/bin/bash

repos=("https://github.com/zsh-users/zsh-autosuggestions.git" "https://github.com/zsh-users/zsh-syntax-highlighting.git" "https://github.com/marlonrichert/zsh-autocomplete.git")

for repo in ${repos[@]}; do
  git clone $repo
  echo ""
done
