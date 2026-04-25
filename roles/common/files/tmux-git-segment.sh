#!/bin/sh

path="${1:-.}"
root=$(git -C "$path" rev-parse --show-toplevel 2>/dev/null) || exit 0
[ -n "$root" ] || exit 0

branch=$(git -C "$path" branch --show-current 2>/dev/null)
[ -z "$branch" ] && branch=$(git -C "$path" rev-parse --short HEAD 2>/dev/null)

printf ' %s ⎇ %s ' "$(basename "$root")" "$branch"
