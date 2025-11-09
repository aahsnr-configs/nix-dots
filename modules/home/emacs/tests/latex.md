```txt
cd /tmp
   tectonic -Z shell-escape-cwd=/tmp --outfmt pdf --outdir /tmp test.tex
   convert -density 140 -trim -antialias test.pdf test.png
```
