#+STARTUP: overview
#+STARTUP: hidestars
#+LANGUAGE: it

* PREPARAZIONE INIZIALE
  Si presuppone un repository centrale remoto e due locali, tipicamante
  portatile e fisso

  https://gist.github.com/joahking/780877

  locale 1
  > mkdir MIAdir
  > cd MIAdir
  > git init

  remoto (potrebbe essere necessario sudo)
  > mkdir remotoBARE
  > cd remotoBARE
  > git --bare init

  locale 1
  Aggiungere qualche file, anche solo il pippo.org generale
  ma anche

  > cp ~/Documenti/BitBucket/Fagus\ Ric/.gtignore . 

  > git add *
  Il comando sopra aggiungerà solo il pippo.org

  > git add -u
  > git commit -m "Primo commit bla bla"
  > git remote add origin /../remotoBARE/
  > git push origin master

  altro locale, locale 2
  > mkdir locale2
  > cd locale2
  > git clone .../remotoBARE

  si ottiene il repository
  .../locale2/remotoBARE

  che a dispetto del nome non è un repository nudo
  Verificabile con 
  > git rev-parse --is-bare-repository 
* Opzioni globali
** .gitgnore

   Sotto alla /home/user/.emacs.d/ mettere un file nascosto
   denominato ".gitignoreGlobale", contenente le righe contenute in

   [[/home/ottorino/.emacs.d/.gitignoreGlobale][questo file]]
  
   che conviene mantenere aggiornato (togliendo gli altri di ogni
   progetto ?).

   Poi da terminale dare il comando
   > git config --global core.excludesfile '~/.emacs.d/.gitignoreGlobale'

   http://stackoverflow.com/questions/7335420/global-git-ignore

   In questo link sotto,

   http://stackoverflow.com/questions/1818895/keep-ignored-files-out-of-git-status

   invece si spiega come fare per togliere dalla lista degli untracked i
   file precedentemente tracciati ma mai commited

   Two things to keep in mind with ignoring files: First, if a file is
   already being tracked by Git, adding the file to .gitignore won’t stop
   Git from tracking it. You’ll need to do 

   > git rm --cached <file> 

   to keep the file in your tree and then ignore it. Secondly, empty
   directories do not get tracked by Git. If you want them to be
   tracked, they need to have something in them. Usually doing a touch
   .gitignore is enough to keep the folder tracked.

   Per verificare cosa è attualmente ignorato
   > git status --ignored
   http://stackoverflow.com/questions/466764/git-command-to-show-which-specific-files-are-ignored-by-gitignore

* stato di molti repositories 
  Da linea di comando, entro la directory Documenti/BitBucket dare

  find . -maxdepth 1 -mindepth 1 -type d -exec sh -c '(echo {} && cd {} && git status -s && echo)' \;

  Vedi
  https://coderwall.com/p/grmruq/git-status-on-all-repos-in-folder

  - find . : to find everything in the current folder
  - maxdepth 1 : so that it doesn't recurse into subdirs of the repos
  - mindepth 1 : so that it skips the current directory (of depth 0)
  - type d : only find directories
  - exec sh -c : spawn a shell and give it a command
    '(echo {} && cd {} && git status && echo)' : the command given to the shell
    - echo {} : echo the directory found by find
    - cd {} : cd into the directory found by find
    - git status -s : run the actual git status, with the -s (short) option
    - echo : echo an empty line, for readability
    ' \; : semicolon to run shell for each of the found directories instead of passing them all to one shell as arguments

* branch
  Crea un branch "Post_ferie_2014" con le ultime modifiche fatte alla
  copia di lavoro [[http://stackoverflow.com/questions/1394797/move-existing-uncommited-work-to-a-new-branch-in-git][Vedi qui]]

  > git checkout -b Post_ferie_2014

  Propaga in su il branch "Anno2016"
  > git push --set-upstream origin Anno2016

  Lista tutti i branch (-all)
  > git branch -a

  Quelle che cominciano con "remotes/origin/" sono sul repository
  remoto, le altre no. Ad esempio

  \* Anno2016
  master
  modifiche_perse
  remotes/origin/Anno2016
  remotes/origin/HEAD -> origin/master
  remotes/origin/master

  "master" e "Anno2016" sono sia sul remoto che sul locale.
  "modifiche perse" è solo in locale.

* commit

  Aggiungi al commit delle cose rimaste fuori e ricordate all'ultimo
  momento [[http://lostechies.com/derickbailey/2010/06/09/git-oops-i-forgot-to-add-those-new-files-before-committing/][Vedi qui]] 

  > git commit --amend –C HEAD

  Aggiungi tutte le modifiche non staged [[https://drupal.org/node/1086094][Vedi qui]]

  > git add -u

  Aggiungi tutti i file aggiunti e non tracked

  > git add *

  Per tornare indietro al commit precedente, magari fatto male o per
  errore

  > git reset --soft HEAD~1

  Per mettere insieme piu' commit prima di un push (interactive)
  [[https://ariejan.net/2011/07/05/git-squash-your-latests-commits-into-one/][Vedi  qui]]

  > git rebase -i

  Dentro una finestra emacs comparira' una cosa tipo;
  pick f392171 Added new feature X
  pick ba9dd9a Added new elements to page design
  pick df71a27 Updated CSS for new elements
  da cambiare in

  pick f392171 Added new feature X
  squash ba9dd9a Added new elements to page design
  squash df71a27 Updated CSS for new elements

  alla fine C-c C-c per eseguire e cambiare il messaggio di commit

* stash 

  [[http://it.gitready.com/beginner/2009/01/10/stashing-your-changes.html][Vedi qui]]

  [[https://git-scm.com/book/no-nb/v1/Git-Tools-Stashing][Pagina del sito ufficiale di git]]

  Stash = Metti da parte, accantona temporaneamente
  > git stash

  Fai una lista degli accantonamenti
  > git stash list

  Eliminare l'ultimo stash
  > git stash drop

  Eliminare un certo stash

  >git stash drop <id>

  > git stash apply

  per ritornare alla condizione di lavoro prima dello stash

** Curiosare nello stash 
   Vedere tutto il contenuto dello stash "uno"
   > git stash show -p stash@{1}

   Vedere solo i nomi dei files
   > git stash show -p stash@{1} --name-only

   Estrarre dallo stash solo il file "git.org" posto sotto la dir
   "messalini" 
   > git checkout stash@{2} -- messalini/git.org

* tag

  git tag -a v1.0 -m "Modifiche dopo il crash SSD del Fujitsu"
  git push origin v1.0

* Varia
** Eliminare tutti i file untracked
   Pulizia file untracked [[http://stackoverflow.com/questions/61212/how-do-i-remove-local-untracked-files-from-my-current-git-branch][Vedi qui]]

   > git clean -f -n 

   per vedere che succederebbe. "n" sta per dry run

   > git clean -f
   per eseguire il comando

   se eseguito sotto la radice git pulisce tutto, altrimenti pulisce la
   dir dove si trova

   ATTENZIONE: cancella i file anche dalla directory di lavoro e non solo da git
   Il comando "git clean -f" si comporta come se fosse un rm da terminale

** Eliminare alcuni i file untracked
   [[http://gitready.com/beginner/2009/01/16/cleaning-up-untracked-files.html][Vedi qui]] 

   > git clean -d -n RelazioniEtAnalisi/#ProveVegan.R# 


** Pulizia generale da file temporanei inutili
   Lanciare questo "oneliner" sotto a ~/Documenti/BitBucket/

   find . -type f \( -name '*.Rnw#' -o -name '*.*#' -o -name '*.*~' \) -delete

   Trovato [[http://unix.stackexchange.com/questions/45800/locate-and-delete-all-temporary-files-in-user-directory][qui]]

** Differenze tra copia attuale e precedenti
   apre gitk e permette di vedere le differenze tra la copia attuale e
   quelle precedenti. Basta cliccare sull'albero in alto a sinistra.

   > gitk /path/to/file

   Recupera il file della versione 0d8.... ___:___ path/to/file e lo
   salva come (>) butta.tex

   > git show 0d8a987e96394d21cdccdf1459536f2b3ed9e2cd:10_lezione/lezione_10.tex>butta.tex

   ATTENZIONE: l'autocompletamento non è ammesso con le SHA-1. Non servirebbe e non
   ce l'hanno messo. Quindi va copiato da git log
   Inoltre fare attenzione al segno ":" tra SHA-1 e path completo al file da recuperare

   Si puo' fare anche meglio da dentro emacs C-x v=, ovvero
   tools -> Version Control -> compare with base version

** Confronta la versione locale col repository
   [[http://stackoverflow.com/questions/5162800/git-diff-between-cloned-and-original-remote-repository][Vedi qui]]
   [[http://stackoverflow.com/questions/1800783/compare-local-git-branch-with-remote-branch][e anche qui]]
   la differenza è tra l'ultimo commit e il repository remoto
   
   passo 1: aggiorna la copia locale col contenuto del remoto senza
   modificare alcunchè sulla copia di lavoro (fetch = andare a prendere)

   > git fetch origin 

   passo 2: confronta la copia locale (master oppure portatile) con
   quella remota tirata giu' da fetch
   
   > git diff master origin/master

   passo 3: aggiorna la copia locale con le modifiche tirate giu' da fetch

   > git merge

   Infatti un git pull equivale a un git fetch seguito da un git merge

** Cerca i file di grandi dimensioni
   Dentro un file dal nome  git_find_big.sh inserire le seguenti linee

   #!/bin/bash
   #set -x 

   # Shows you the largest objects in your repo's pack file.
   # Written for osx.
   #
   # @see http://stubbisms.wordpress.com/2009/07/10/git-script-to-show-largest-pack-objects-and-trim-your-waist-line/
   # @author Antony Stubbs

   # set the internal field spereator to line break, so that we can iterate easily over the verify-pack output
   IFS=$'\n';

   # list all objects including their size, sort by size, take top 10
   objects=`git verify-pack -v .git/objects/pack/pack-*.idx | grep -v chain | sort -k3nr | head`

   echo "All sizes are in kB's. The pack column is the size of the object, compressed, inside the pack file."

   output="size,pack,SHA,location"
   for y in $objects
   do
   # extract the size in bytes
   size=$((`echo $y | cut -f 5 -d ' '`/1024))
   # extract the compressed size in bytes
   compressedSize=$((`echo $y | cut -f 6 -d ' '`/1024))
   # extract the SHA
   sha=`echo $y | cut -f 1 -d ' '`
   # find the objects location in the repository tree
   other=`git rev-list --all --objects | grep $sha`
   #lineBreak=`echo -e "\n"`
   output="${output}\n${size},${compressedSize},${other}"
   done

   echo -e $output | column -t -s ', '

* Passa a una versione precedente
  Mettiamo che siamo sul ramo "pippo": se si vuole tornare indietro
  nella storia mentre si sta lavorando procedere come segue:

  Mettere da parte il lavoro attuale con 

  > git stash

  Il comando sopra mette da parte tutto quello che é stato fatto e
  torna allo stato precedente

  Si può adesso lavorare sulla versione precedente.
  Adesso ci si puo' spostare dal ramo "pippo" al ramo "pluto" con

  > git checkout "pluto"

  e andare a vedere come stavano le cose in un momento precedente.

  Alla fine si da il comando 

  > git stash apply

  per ritornare alla condizione di lavoro prima dello stash

* Passa a commit precedenti

  La sequenza dovrebbe essere questa:
  > git branch
  Per prendere nota su quale branch siamo e per tornarci alla fine delle prove. Mettiamo si chiami "mioRamo"
  > git stash
  Per mettere da parte le eventuali modifiche ancora non inserite in un commit
  > git log
  Per prendere nota degli SHA-1 dei commmit precedenti (vedi quanto scritto in [[Differenze tra copia attuale e precedenti]] a proposito dell'autocompletamento)
  > git checkout e25a2d26cea6fccb12607f42a482a721313ba7f7
  Per riportare la situazione al momento del commit e25a... e sperimentare
  > git checkout MioRamo
  Per tornare alla situazione di dove siamo partiti
  Infine 
  > git stash apply
  Per recuperare le modifiche non ancora inserite in un commit e messe da parte all'inzio di questa procedura 
  Ulteriori dettagli sulla procedura si trovano [[https://www.atlassian.com/git/tutorials/undoing-changes][qui]].

* Cambio di repository remoto

  [[https://help.github.com/articles/changing-a-remote-s-url/][Vedi in rete]]

  > cd ~/Documenti/BitBucket/GIT_corso_vivaistica
  > mkdir /mnt/D098970/BARE_VIVA
  > cd /mnt/D098970/BARE_VIVA/
  > git init --bare
  > cd ~/Documenti/BitBucket/GIT_corso_vivaistica
  > git remote set-url origin /mnt/D098970/BARE_VIVA/
  > git push

  Il comando

  > git remote set-url origin /mnt/D098970/BARE_VIVA/

  va in seguito dato anche sugli altri computer (tipicamente un
  portatile) seguito da

  > git pull

  In modo da sincronizzare tutto il sistema di subversionamento
