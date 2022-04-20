Jupyter.keyboard_manager.command_shortcuts.add_shortcut('0,1', function (event) {
    Jupyter.notebook.kernel.restart();
    restartTime = 500 // decrease this if you have a fast computer
    setTimeout(function(){ Jupyter.notebook.execute_all_cells(); }, restartTime);
    return false;
});
