set -l TRASH_DIR "$HOME/.local/share/trash/files"

function main
    if test (count $argv) -eq 0
        echo "Error: No files or directories specified." >&2
        echo "Usage: (status filename) <file1> [<file2> ...]" >&2
        exit 1
    end

    set -l targets
    for item in $argv
        if not test -e "$item"
            echo "Warning: '$item' not found, skipping." >&2
            continue
        end

        mkdir -p "$TRASH_DIR"
        if test (realpath -- "$item") = (realpath -- "$TRASH_DIR")
            echo "Warning: Cannot move the trash directory into itself. Skipping '$item'." >&2
            continue
        end
        set -a targets "$item"
    end

    if test (count $targets) -eq 0
        echo "No valid files or directories to move to trash."
        exit 0
    end

    echo "The following items will be moved to the trash:"
    printf "  - %s\n" $targets
    echo "Trash location: $TRASH_DIR"
    echo ""

    read -p "Are you sure you want to continue? (Y/n): " -l REPLY
    echo ""

    if test -n "$REPLY"
        echo "Operation cancelled by user."
        exit 1
    end

    echo "Moving items to trash..."
    for item in $targets
        set -l trashed_name (basename "$item")-(date +%s)-(head -c 5 /dev/urandom | tr -dc A-Za-z-0-9)
        echo " - '$item' -> '$trashed_name'"
        mv -- "$item" "$TRASH_DIR/$trashed_name"
    end

    echo ""
    echo "✅ Operation complete."
    echo "To restore, check the trash directory: $TRASH_DIR"
end

main $argv
