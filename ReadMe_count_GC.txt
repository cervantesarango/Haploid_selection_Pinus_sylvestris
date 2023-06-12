# estimate GC content of sequences
# code comes from https://www.biostars.org/p/300870/#476438


perl -lane 'unless (/^>/) { $l += length(); $gc++ while /[GC]/ig } END { print $gc/$l}' sequence.fa 
 
