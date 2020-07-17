. "$ZDOTDIR/.zshenv"

# TODO Load theme without OMZ
ZSH_THEME="${THEME:-novcs-agnoster}"

# Use case-sensitive completion.
# CASE_SENSITIVE="true"

# Use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Pasting URLs and other text is messed up?
# DISABLE_MAGIC_FUNCTIONS=true

# Disable colors in ls.
# DISABLE_LS_COLORS="true"

# Disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Enable command auto-correction.
# ENABLE_CORRECTION="true"

# Display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Don't add commands starting with space to history
setopt histignorespace

# Plugins to load
plugins=(
	archlinux
	vi-mode
)

. $ZSH/oh-my-zsh.sh

# Auto add every module in $ZSH_PLUGIND
for dir in $ZSH_PLUGIND/*; do
    name="$(echo $dir | grep -oP '.*/\K.*$')"
    [ -f "$dir/$name.zsh" ] && . "$dir/$name.zsh" || . "$dir/$name.plugin.zsh"
done
