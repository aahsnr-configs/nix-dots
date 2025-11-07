{pkgs, ...}: {
  home.packages = with pkgs; [
    basedpyright
    ruff
    mutmut
    (python313.withPackages (python-pkgs:
      with python-pkgs; [
        pandas
        requests
        jupyter
        numpy
        pandas
        matplotlib
        scipy
        seaborn
        scikit-learn
        requests
        pillow
        pip
        pytest
        debugpy
        hypothesis
        coverage
        pytest-cov
        pytest-benchmark
        pytest-xdist
      ]))
  ];
}
