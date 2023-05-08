# Team's KIDS23 BioHackathon virtual machine is up and running

Depending on your needs, there are multiple avenues for you to access and utilize this VM:

## RStudio

[http://20.225.156.182/](http://20.225.156.182/)
Login with your username (first name initial + last name, e.g. “jsmith” for John Smith).
Users should sign into RStudio with their username, as multiple sessions for the same user are not allowed.
Your team's usernames are listed below (quotes not needed for use):

## Jupyterhub

[https://20.225.156.182:8000/](https://20.225.156.182:8000/)
Login with lead as the username - this is the login everyone should use for JupyterHub.
It doesn't seem to matter if multiple people are using it concurrently.
Both python and R kernels are available for use by default.

Note that your browser may complain about the site being unsafe due to its security certificate being signed by itself.
You can safely proceed to the site.

## SSH into the VM

Important note for internal people: This will ONLY work from the login nodes of the HPC on secure networks for security reasons. If you want to connect directly from your local machine, you must be connected to HOPENET rather than SJCRH or SecureNET.

```bash
ssh lead@your.vm.ip.here
```

Sudo access is granted, so feel free to install software or configure your VM as needed.

To copy files to the VM use scp:

```bash
scp file.txt remote_username@your.vm.ip.here:/home/remote_username
```

## A few things of notes

- Packages installed in RStudio will be installed for all users.
- To install python/conda packages from within a Jupyter notebook, you can do so by running the following in a code cell:

```python
import sys
! {sys.executable} -m pip install numpy
```

Or for conda:

```python
import sys
! {sys.prefix}/bin/conda install --yes --prefix {sys.prefix} numpy
```

- If you want to install R packages from Jupyter, it's best to start a terminal from Jupyter, start R, and install packages as usual. They will then be available to the kernel.
- Resources (4 CPUs, 16 GB RAM by default) for each team are shared between users. If 6 people are all trying to run stuff at once, you may run into issues.
  - If your team needs other resources, please let me know ASAP.
