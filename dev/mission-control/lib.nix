{ execPaths, mktemp, ... }:
{
  categories = {
    builds = "Builds";
    checks = "Checks";
    deployments = "Deployments";
    formats = "Formats";
    generators = "Generators";
    previews = "Previews";
    processors = "Process";
    publishes = "Publishes";
  };
  script = dir: body: ''
    set -x
    set -e
    set -o pipefail
    shopt -s globstar

    if [[ -v GH_TOKEN ]]; then
      is_ci="true"
    else
      is_ci="false"
    fi

    # disable caching
    # XDG_CACHE_HOME=$(${mktemp} -d)
    # export XDG_CACHE_HOME 

    export is_ci

    export PATH=''${PATH}:${execPaths}

    cd ${dir}
    ${body}
  '';
}
