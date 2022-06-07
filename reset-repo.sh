git checkout --orphan TEMP_BRANCH

# Add all the files:
git add -A

# Commit the changes:
git commit -am "reset repo"

# Delete the old branch:
git branch -D main

# Rename the temporary branch to master:
git branch -m main

# Finally, force update to our repository:
git push -f origin main