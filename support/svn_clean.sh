#! /bin/sh

# $Id: svn_clean.sh 6391 2009-04-21 08:12:37Z zalila $

work_dir="`pwd`"

# Clean the runtimes

for r in "polyorb-hi-ada" "polyorb-qos-ada" "polyorb-hi-c"; do
    test -d "resources/runtime/${r}" || continue
    
    echo "Cleaning runtime ${r}"
    cd "resources/runtime/${r}"
    
    svn status --no-ignore | grep "^[?I]" \
	| awk '{print $2}' | xargs -I{} chmod -R u+w {}
    svn status --no-ignore | grep "^[?I]" \
	| awk '{print $2}' | xargs rm -rf

    cd "${work_dir}"
done

# Clean the remaining without deleting the runtimes

runtime_exclude_pattern='resources/runtime/\(polyorb\-hi\-ada\|polyorb\-qos\-ada\|polyorb\-hi\-c\)'

svn status --no-ignore \
    | grep "^[?I]" \
    | grep -v "${runtime_exclude_pattern}" \
    | awk '{print $2}' \
    | xargs -I{} chmod -R u+w {}
svn status --no-ignore \
    | grep "^[?I]" \
    | grep -v "${runtime_exclude_pattern}" \
    | awk '{print $2}' \
    | xargs rm -rf

echo "Done."
