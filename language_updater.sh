#!/bin/bash -e

mandimusdir=~/.config/mandimus
sentences=$mandimusdir/sentences.corpus
langdir=$mandimusdir/language
tempfile=$mandimusdir/url.txt
lmtoolurl=http://www.speech.cs.cmu.edu/cgi-bin/tools/lmtool/run

cd $mandimusdir

# upload corpus file, find the resulting dictionary file url
curl -L -F corpus=@"$sentences" -F formtype=simple $lmtoolurl \
  |grep -A 1 "base name" |grep http \
  | sed -e 's/^.*\="//' | sed -e 's/\.tgz.*$//' | sed -e 's/TAR//' > $tempfile

# download the .dic and .lm files
curl -C - -O $(cat $tempfile).dic
curl -C - -O $(cat $tempfile).lm

# mv em to the right name/place
mv *.dic $langdir/dic
mv *.lm $langdir/lm

rm $tempfile
