set history save on
set print pretty on
set pagination off
set confirm off

python
import sys
import pathlib
boost_pp_path = "/opt/Boost-Pretty-Printer"
if pathlib.Path(boost_pp_path).exists():
  sys.path.insert(1, boost_pp_path)
  __import__('boost').register_printers(boost_version=(1, 77, 0))
end
