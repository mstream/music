{
  categories,
  geminiCli,
  git,
  nix,
  script,
  ...
}:
let
  ai = model: {
    description = "Run AI CLI (${model} model) in the application context";
    exec = script ''
      SEATBELT_PROFILE=custom ${geminiCli} --model ${model} 
    '';
  };
in
{
  ai-fast = ai "gemini-3-flash-preview";
  ai-thorough = ai "gemini-3-pro-preview";
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
