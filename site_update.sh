#!/bin/bash

cp /Users/naskuv/Dropbox/Professorship/vitae/Yizhen_Huang_CV_web.pdf
hugo
read -p "Enter commit message: " commit_message
git add .
git commit -m "$commit_message"
git push upstream master
netlify deploy --prod
echo "eejain site updated!"
