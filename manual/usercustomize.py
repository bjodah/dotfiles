# Put in this file (usercustomize.py) in "site.USER_SITE"
import sys, os, site
for idx, pth in enumerate(sys.path):
    if pth.startswith('/usr'):
        sys.path.insert(idx, site.USER_SITE)
        break
else:
    raise ValueError("No path starting with /usr in sys.path")
