# Connecting to GitHub

To push your local Git repository to GitHub:

1. Create a new repository on GitHub
   - Go to https://github.com/new
   - Enter a repository name (e.g., "crime-location-choice-studies")
   - Add an optional description
   - Choose public or private repository
   - Do NOT initialize with README, .gitignore, or license (since you already have these locally)
   - Click "Create repository"

2. Copy the repository URL from GitHub
   - It will look like: `https://github.com/yourusername/crime-location-choice-studies.git`

3. Connect your local repository to GitHub
   - Run the following commands in your terminal:
   ```
   git remote add origin https://github.com/yourusername/crime-location-choice-studies.git
   git branch -M main
   git push -u origin main
   ```

4. Enter your GitHub credentials when prompted

Your code is now on GitHub!
