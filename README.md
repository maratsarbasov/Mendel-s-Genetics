## MendellianGenetics
Haskell library for simulating Mendel's genetics. Written in Haskell.
### Usage
To get all possible children from 2 parents:
```
getChildren (stringToGenotype "AaccDD") (stringToGenotype "aACcDD")
```
Result:
```
Prob [(AaCcDD,0.25),(AaccDD,0.25),(aaCcDD,0.125),(aaccDD,0.125),(AACcDD,0.125),(AAccDD,0.125)]
```
