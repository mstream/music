{
  categories,
  git,
  nix,
  script,
  ...
}:
{
  check-all = {
    category = categories.checks;
    description = "Check all files";
    exec = script ''
      run app-check
      ${nix} flake check
    '';
  };
  format-all = {
    category = categories.formats;
    description = "Format all files";
    exec = script ''
      run app-format
      ${nix} fmt
    '';
  };
  git-sync = {
    description = "Synchronize with the remote default branch";
    exec = script ''
      ${git} fetch --all --tags && git checkout origin/main
    '';
  };
}
