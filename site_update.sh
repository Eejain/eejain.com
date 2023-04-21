#!/bin/bash

cp /Users/naskuv/Dropbox/Job/vitae/Yizhen_Huang_CV.pdf /Users/naskuv/eejainsite/content/CV/Yizhen_Huang_CV.pdf
hugo
read -p "Enter commit message: " commit_message
git add .
git commit -m "$commit_message"
git push upstream master
netlify deploy --prod
echo "eejain site updated!"
