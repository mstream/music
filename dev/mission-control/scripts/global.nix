{
  categories,
  geminiCli,
  git,
  nix,
  script,
  ...
}:
{
  ai = {
    description = "Run AI CLI in the application context";
    exec = script ''
      SEATBELT_PROFILE=custom ${geminiCli} --model gemini-3-flash-preview 
    '';
  };
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
  update-flake-inputs = {
    description = "Update Nix flake inputs";
    exec = script ''
      ${nix} flake update
      cd dev && ${nix} flake update
    '';
  };
}
