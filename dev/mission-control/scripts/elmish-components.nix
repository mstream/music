{
  categories,
  httpServer,
  script,
  spago,
  watchexec,
  ...
}:
let
  dir = "packages/elmish-components";
in
{
  elmish-components-build = {
    category = categories.builds;
    description = "Build elmish-components application";
    exec = script ''
      rm -f ${dir}/index.js
      ${spago} bundle \
        --bundle-type module \
        --module Elmish.Components.Main \
        --package elmish-components \
        --platform browser
    '';
  };
  elmish-components-preview = {
    category = categories.previews;
    description = "Preview elmish-components website";
    exec = script ''
      ${httpServer} ${dir} &
      trap "kill %1" EXIT
      ${watchexec} --exts html,js,purs,yaml --print-events -- "run elmish-components-build; echo Ctrl-C to exit"
    '';
  };
}
