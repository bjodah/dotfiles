Custom keyboard shortcuts:

.. code-block:: console

   $ #gsettings set org.gnome.desktop.input-sources xkb-options ['ctrl:nocaps']
   $ gsettings set org.gnome.desktop.input-sources xkb-options "['caps:escape']"

https://github.com/sezanzeb/input-remapper

Stale customizations
====================
- xonsh: Ctrl+Alt+R
    - gnome-terminal -e "bash -c 'source ~/use_miniconda3_root.sh;xonsh'"

Manual installation of prelude
==============================
.. code-block:: shell-session

    $ PRELUDE_INSTALL_DIR="$HOME/.emacs.prelude" ~/.&& curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh

Misc commnads
=============

.. code-block:: shell-session

   $ ssh -Y 192.168.1.140 'bash -c "source ~/.profile; emx --use 29 -- --x11 -v /mnt:/mnt"'
