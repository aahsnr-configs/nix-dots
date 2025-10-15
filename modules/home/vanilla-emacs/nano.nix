{
  fetchFromGitHub,
  trivialBuild,
}:

# ============================================================================
# Custom Emacs Package: nano-theme
# ============================================================================
# This derivation packages the nano-theme directly from GitHub.
# Pattern follows best practices from emacs-overlay and nixpkgs.
#
# nano-theme: A minimalist theme inspired by the N Λ N O design philosophy
# Repository: https://github.com/rougier/nano-theme
# ============================================================================

trivialBuild rec {
  pname = "nano-theme";
  version = "2023-03-15";

  # Fetch source from GitHub
  src = fetchFromGitHub {
    owner = "rougier";
    repo = "nano-theme";
    rev = "29ae0c935c7c9c9be00c948c56a43c5c458f4e3d"; # Pin to specific commit
    sha256 = "sha256-3FkVLbPWY+3PjzUj5S8FvYvDC7tKNXlbGiAFwHx8vKw=";
  };

  # This package has no dependencies beyond Emacs itself
  # If it did, we would list them here:
  # propagatedUserEnvPkgs = [ dependency1 dependency2 ];
  # buildInputs = propagatedUserEnvPkgs;

  # Optional: Add metadata for better package documentation
  meta = {
    description = "Nano theme for Emacs - A minimalist, elegant color scheme";
    homepage = "https://github.com/rougier/nano-theme";
    license = "GPL-3.0-or-later"; # Check actual license in repository
    maintainers = [ ]; # Add your name if maintaining this locally
  };
}
