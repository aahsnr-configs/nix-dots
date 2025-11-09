Assuming that I changed my vanilla emacs configuration and that this
configuration is now correctly doing what I intended in my previous query modify
the citar section so that I can reference multiple bib files but those
bibliography are project specific. Assuming that I want my latex writing and
python programming using a single directory with multiple sub-folders, make sure
that my emacs configuration is well suited for scientific  computing using
python and scientific writing using latex, both of these tasks inside org mode
files as individual .tex files and .py, .ipynb files inside subfolders. The
python programming inside org-mode files would be done using the emacs jupyter
package from `https://github.com/emacs-jupyter/jupyter`. I also want to
following setup for my configuration:

```el
(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                    (:session . "py")
                                                    (:kernel . "python3")))
```

And then add the following config

```el
(org-babel-jupyter-override-src-block "python")
```

so that jupyter notebook behaviour is automatically picked up in python source
code blocks

This emacs configuration current does not have a configuration for the emacs ein
package for opening ipynb files. Write a configuration for this package
following usingmy emacs configuration.

I specifically want to make use of `direnv` with `.envrc` inside project root
with .envrc containing `use flake` and the attached flake.nix file. Keep in mind
that the texlive related packages is installed as a home-manager module in the
attached `default.nix` file.

Also search the web and determine if there are evil extensions that are
necessarily bneeeded for vanilla emacs configuration.

Furthermore, the lsp-brigde completion in my vanilla emacs configuration is
messing up with entering org structure templates using <el followed by TAB key
to input the org-structure-template for elisp and other templates. Fix this
issue as well.

# Only on the last review:

- find any duplicate, unneeded or overlapping configurations from different
  packages
- prevent search word from appearing as completion candidates for lsp-bridge as
  it lowers my typing speed and explain what purpose it serves by studying the
  lsp-bridge github page's readme in
  `https://raw.githubusercontent.com/manateelazycat/lsp-bridge/refs/heads/master/README.md`
- fix typing lag in org-mode
- determine if it might be beneficial to make use of emacs' built-in project.el
  package might be more

# Questions

For my attached vanilla emacs configuration file `config.org` managed using
NixOS-commmunity's emacs-overlay as outlined in the attached `default.nix`,
answer the following questions and perform any mentioned tasks, and then only
write out the changes needed. Everything should be written in a single markdown
file with markdown code blocsk. For everything, search the web before writing
anything:

- [ ] Setting `(org-src-fontify-natively nil)` disables syntax highlighting in
      org source code blocks but keep in mind that lsp-bridge is turned on and
      active in org source code blocks. Search the web and determine if it is
      possible to re-enable syntax-highlighting in org source code blocks using
      treesit instead? If yes, then implement a solution
- [ ] Are there any missing packages or configuration for default vim-like
      behaviour in this configuration?
- [ ] There are many issues with the evil implementation in my vanilla emacs
      configuration. Search through the doom emacs project in
      https://github.com/doomemacs/doomemacs to implement its evil
      implementation as closely as possible.
- [ ] Implement doom emacs word wrap setup
- [ ] Does my current config have any lingering projectile configuration?
- [ ] Will my attached `flake.nix` and my emacs configuration's envrc setup
      allow trhe libraries and packages mentioned in the `flake.nix` file to be
      detected in subfolders that are within the same folder as the `flake.nix`
      file?
- [ ] Is it possible to add :tangle to
      org-babel-default-header-args:jupyter-python in the jupyter configuration
      section? If yes to this question, then add a default .py file. But since
      my org files and python files will be project specific, is it possible to
      dynamically tangle a jupyter-python/python source code block to a specific
      python file that I name myself. If yes then write a function to do this
      action and add to the top of the header arguments of a particular file.
- [ ] All projects using the project.el package should be directed to ~/Projects
      directory in linux. Individual projects will be subfolders in this
      particular directory. Make the necessary changes to project.el
      configuration accordingly.
- [ ] Since an org file can have multiple #+Property with their distinct header
      arguments, is it possible for emacs to sort the different #+Property in
      org source code blocks depending on the type of org source code block. To
      simplify what I mean, Let's say I have a uniqure project in the
      `sample-project` folder in the `~/Projects` directory. This subfolder will
      contain types of files, including but not limited to `flake.nix`,
      `.envrc`, `.py` files, `.tex` files and, of course, org-mode files. Keep
      in mind that each org-mode file will serve a specific subproject in the
      main project and it will be used to write scientific documents using latex
      as well doing python programming using jupyter inside the same org-mode
      file. Of course, when exporting the whole org-mode file to tex file and
      then to pdf file, the python code will included but without the respective
      output from python/jupyter-python source code blocks. But when tangling
      the org-mode file, only the python source code block would be tangled to a
      .py file to a python subfolder in the same folder as the org file. The org
      file will be exported to a pdf file. When I write either a latex source
      code block or a python source code block, each of the 2 types will have 2
      respectively different header arguments associated with 2 respective
      `#+Property`. When I write either of these of source code blocks, I only
      want to include where its a python or latex source code block like
      `#+begin_src python` or `#+begin_src latex`, but leave out the respective
      individual header arguments. The headers arguments will be picked up from
      the respective `#+Property` lines at the top of the org file based on
      whether I write `python` or `latex` in the source code block. Search the
      web as well as the emacs documentation for a sophisticated implementation
      for this secenario. If the scenario I explain needs improving for better
      productivity, without loosing the core idea behind it, implement a
      different solution. I leave that upto you. Also account for a scenario
      where I don't have to export the python source code blocks to a pdf file.
- [ ] I have set the following configuration options:
      `(setq-local
bidi-paragraph-direction 'left-to-right)`,
      `(setq-local bidi-inhibit-bpa
t)` - instead of setting them with org-mode
      using a lambda function. Does my method achieve the same performance
      optimizations.?
- [ ] For the `Jupyter Configuration` of my emacs configuration file, is the
      `(with-eval-after-load 'ob-jupyter` necessary?

Repeat the previous task, but focus more on the  following parts of the previous
prompt:

- [ ] There are many issues with the evil implementation in my vanilla emacs
      configuration. Search through the doom emacs project in
      https://github.com/doomemacs/doomemacs to implement its evil
      implementation as closely as possible.
- [ ] Implement doom emacs word wrap setup

- [ ] Since an org file can have multiple #+Property with their distinct header
      arguments, is it possible for emacs to sort the different #+Property in
      org source code blocks depending on the type of org source code block. To
      simplify what I mean, Let's say I have a uniqure project in the
      `sample-project` folder in the `~/Projects` directory. This subfolder will
      contain types of files, including but not limited to `flake.nix`,
      `.envrc`, `.py` files, `.tex` files and, of course, org-mode files. Keep
      in mind that each org-mode file will serve a specific subproject in the
      main project and it will be used to write scientific documents using latex
      as well doing python programming using jupyter inside the same org-mode
      file. Of course, when exporting the whole org-mode file to tex file and
      then to pdf file, the python code will included but without the respective
      output from python/jupyter-python source code blocks. But when tangling
      the org-mode file, only the python source code block would be tangled to a
      .py file to a python subfolder in the same folder as the org file. The org
      file will be exported to a pdf file. When I write either a latex source
      code block or a python source code block, each of the 2 types will have 2
      respectively different header arguments associated with 2 respective
      `#+Property`. When I write either of these of source code blocks, I only
      want to include where its a python or latex source code block like
      `#+begin_src python` or `#+begin_src latex`, but leave out the respective
      individual header arguments. The headers arguments will be picked up from
      the respective `#+Property` lines at the top of the org file based on
      whether I write `python` or `latex` in the source code block. Search the
      web as well as the emacs documentation for a sophisticated implementation
      for this secenario. If the scenario I explain needs improving for better
      productivity, without loosing the core idea behind it, implement a
      different solution. I leave that upto you. Also account for a scenario
      where I don't have to export the python source code blocks to a pdf file.

- [ ] Make sure the Project-specific citar configuration also works in org-mode
      files.

- [ ] There are many issues with the evil implementation in my vanilla emacs
      configuration. Search through the doom emacs project in
      https://github.com/doomemacs/doomemacs to implement its evil
      implementation as closely as possible.
- [ ] Implement doom emacs word wrap setup

But also implement doom emacs's smartparens implementation and provide with
instructions on how to use inheritance with direnv and .envrc using `use flake`

Then update the existing markdown file with any changes.

My emacs configuration is stored in ahsanur041@proton.me

[without longer thinking] I basically queried with 2 prompts. Write 2 separate
markdown files distinguishing so I can tell which prompt resulted in what
response in the markdown. I know version 11 of the markdown output is the
response from the 2nd prompt. Write another markdown file that contains the
final form of the response from the 1st prompt. Furthermore the version 11 of
the markdown output of the 2nd prompt is not properly formatted. There are
errors in this markdown output. Make sure both the markdown output are
well-formatted and are labelled 1st-prompt-markdown-output and
2nd-prompt-markdown-output.

[without longer thinking] Study my newly attached emacs configuration and
determined what this green color comes from. Help me debug this. This green
color should not come from the doom themes package or the theming setup in my
emacs configuration

[with longer thinking] Without using external emacs modeline package, modify the
built-in emacs modeline with external emacs packages (not modeline package like
doom-modeline, telephone-line) to have features, functionalities and aesthetics
that mimic the modeline configuration from popular neovim distributions like
nvchad and astronvim. The modeline configuration must also make use of nerd
icons. It must also have quality-of-life features for use in emacs that are
commonly found in emacs packages such doom-modeline. Furthermore the modeline's
theming must match the color palette of the catppuccin mocha theme
