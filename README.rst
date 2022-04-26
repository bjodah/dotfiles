Custom keyboard shortcuts:

.. code-block:: console

   $ #gsettings set org.gnome.desktop.input-sources xkb-options ['ctrl:nocaps']
   $ gsettings set org.gnome.desktop.input-sources xkb-options "['caps:escape']"


Stale customizations
====================
- xonsh: Ctrl+Alt+R
    - gnome-terminal -e "bash -c 'source ~/use_miniconda3_root.sh;xonsh'"


Misc commnads
=============

.. code-block:: shell-session

   $ ssh -Y 192.168.1.140 'bash -c "source ~/.profile; emx --use 29 -- --x11 -v /mnt:/mnt"'
