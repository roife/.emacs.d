function ezf --description 'Select piped candidates with Emacs completion'
    argparse \
        h/help \
        i/insert \
        z/read0 \
        Z/print0 \
        q/query= \
        f/field= \
        -- $argv
    or return

    if set -q _flag_help
        printf '%s\n' \
            'usage: COMMAND | ezf [OPTIONS]' \
            '' \
            '  -q, --query TEXT   start with TEXT as the filter' \
            '  -f, --field RANGE  return a zero-based field (1, 1-6, or 1-)' \
            '  -z, --read0        read NUL-delimited candidates' \
            '  -Z, --print0       print NUL-delimited selections' \
            '  -i, --insert       replace the current Fish command line'
        return
    end

    set -l candidates (mktemp)
    command cat >$candidates
    set -q _flag_field; or set _flag_field ''
    set -q _flag_query; or set _flag_query ''
    set -l separator line
    set -q _flag_read0; and set separator nul

    set -l selections (__ezf-call generic "$candidates" "$_flag_field" \
        "$_flag_query" "$separator" | string split0)
    command rm -f -- $candidates
    set -q selections[1]; or return 1

    if set -q _flag_insert
        commandline -- (string join ' ' -- $selections)
        commandline -f repaint
    else if set -q _flag_print0
        string join0 -- $selections
    else
        printf '%s\n' $selections
    end
end

function __ezf-call
    set -l request (mktemp)
    set -l result (mktemp)
    string join0 -- $argv >$request

    command emacsclient -nw -u -e \
        "(ezf-client \"$request\" \"$result\")" <&2 >&2

    set -l exit_status 1
    if test -s $result
        command cat $result
        set exit_status 0
    end
    command rm -f -- $request $result
    return $exit_status
end

function ezf-file-widget --description 'Paste selected files and directories onto the command line'
    set -l query (commandline --current-token --tokens-expanded | string collect)
    set -l selection (__ezf-call file "$PWD" "$query" | string split0)

    if set -q selection[1]
        commandline -rt -- (string join ' ' -- (string escape -n -- $selection))' '
    end
    commandline -f repaint
end

function ezf-history-widget --description 'Paste the selected history entry onto the command line'
    set -l command_line (commandline)
    set -l current_line (commandline -L)
    set -l query $command_line[$current_line]

    test -z "$fish_private_mode"; and builtin history merge
    set -l history_file (mktemp)
    builtin history -z >$history_file
    set -l selection (__ezf-call history "$history_file" "$query" | string split0)
    command rm -f -- $history_file

    if set -q selection[1]
        if test (count $command_line) -eq 1
            commandline -- "$selection[1]"
        else
            set -l before (math $current_line - 1)
            set -l after (math $current_line + 1)
            commandline -- $command_line[1..$before] "$selection[1]"
            commandline -a -- '' $command_line[$after..-1]
        end
    end
    commandline -f repaint
end

function ezf-cd-widget --description 'Change into the selected directory'
    set -l query (commandline --current-token --tokens-expanded | string collect)
    set -l selection (__ezf-call directory "$PWD" "$query" | string split0)

    if set -q selection[1]
        cd -- "$selection[1]"
        commandline -rt -- ''
    end
    commandline -f repaint
end

if status is-interactive
    bind \ct ezf-file-widget
    bind -M insert \ct ezf-file-widget
    bind \cr ezf-history-widget
    bind -M insert \cr ezf-history-widget
    bind \ec ezf-cd-widget
    bind -M insert \ec ezf-cd-widget
end
