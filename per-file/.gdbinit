set history save on
set print pretty on
set pagination off
set confirm off
set python print-stack full

python
import sys
import pathlib

boost_pp_path = next(pathlib.Path('/').glob("opt*/Boost-Pretty-Printer"))
if pathlib.Path(boost_pp_path).exists():
  sys.path.insert(1, str(boost_pp_path))
  __import__('boost').register_printers(boost_version=(1, 77, 0))



eigengdb_path = next(pathlib.Path('/').glob("opt*/eigengdb"))
if pathlib.Path(eigengdb_path).exists():
  sys.path.insert(1, str(eigengdb_path))
  __import__('eigengdb').register_eigen_printers(None)
end
