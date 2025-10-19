set -l NVIM_DIRS \
    "$HOME/.config/nvim" \
    "$HOME/.local/state/nvim" \
    "$HOME/.local/share/nvim"

function main
    echo "This script will permanently delete the following Neovim directories:"
    for dir in $NVIM_DIRS
        echo "  - $dir"
    end
    echo ""

    read -p "Are you sure you want to continue? (Y/n): " -l REPLY
    echo ""

    if test -n "$REPLY"
        echo "Operation cancelled by user."
        exit 1
    end

    echo ""
    echo "Starting removal process..."

    for dir in $NVIM_DIRS
        if test -d "$dir"
            echo "Removing $dir..."
            rm -rf "$dir"
        else
            echo "Directory $dir not found, skipping."
        end
    end

    echo ""
    echo "Successfully removed Neovim directories."
end

main
