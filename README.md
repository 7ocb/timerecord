Tool to record time and generate reports.

To build, you will need to install [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) tool.

Then do in the timerecord folder:

```bash
stack build
```

the executable will be placed in 

```
.stack-work/dist/...../build/timerecord
```

usage:

```bash
timerecord help
```

for showing list of available commands

```bash
timerecord help command
```

will display help for specific command

typical workflow is: 

when you start working, you do:

```bash
timerecord enter
```

when you leave work, you do:

```bash
timerecord leave
```

you can register task you did by 

```bash
timerecord did some task description
```

and this can be used for generating monthly timesheet