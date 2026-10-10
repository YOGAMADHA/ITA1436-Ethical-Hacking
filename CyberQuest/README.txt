CYBERQUEST ARCADE UPGRADE
=========================

This ZIP contains a complete Flask implementation with an arcade-style UI and persistent SQLite rewards.

FEATURES
- Animated arcade backgrounds and mission cards
- Correct-answer celebration popup, confetti and XP animation
- Persistent XP and completion history
- Achievement badges stored in SQLite
- 30% chance of a mystery gift after a correct mission (bonus XP saved in SQLite)
- Daily and longest streak tracking
- Unlockable avatars
- Optional synthesized sound effects with mute/unmute
- 8 worlds and 40 cybersecurity missions (5 levels per world)
- Sequential level unlocking: clear each mission to unlock the next
- Leaderboard and profile pages
- Additive table/column setup; no DROP TABLE statements

IMPORTANT DATABASE NOTE
-----------------------
Because the original database.py and schema were not supplied, this package uses its own safe database connection and looks for these existing files in order:
1. cyberquest.db in the project folder
2. database.db in the project folder
3. instance/cyberquest.db
If none exists, it creates instance/cyberquest.db. Before overlaying app.py, make a backup of your current project and database. If your current app uses another database filename, update DB_PATH near the top of app.py to point to that exact file. The campaign seed adds missing mission titles and updates those campaign titles to the bundled educational questions; unrelated mission records are preserved. Do not delete your existing database.

INSTALL / RUN (Windows VS Code PowerShell)
------------------------------------------
1. Extract this ZIP.
2. Back up your current CyberQuest folder and SQLite database.
3. Copy the files from this ZIP into your existing CyberQuest project folder, keeping the folders (templates, static) intact. Allow replacement only after the backup.
4. Open the CyberQuest folder in VS Code.
5. In the VS Code terminal run:
   python -m venv venv
   .\venv\Scripts\Activate.ps1
   python -m pip install -r requirements.txt
   python app.py
6. Open http://127.0.0.1:5000

If PowerShell blocks activation, run this once in that terminal:
Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
Then activate the venv again.

DATABASE COMPATIBILITY
----------------------
This package expects missions columns named: id, title, world, topic, description, question, options, answer, explanation, xp_reward.
It expects users columns id, username, password, xp, created_at, and completions columns user_id, mission_id, score. It adds reward columns/tables if missing and never intentionally drops existing data. However, databases with different column names or a different password format may need a small adapter. Keep your backup until you have verified login, missions, XP and rewards.

SECURITY NOTES
--------------
- Change the development secret key using the CYBERQUEST_SECRET environment variable before deployment.
- Use HTTPS and proper authorization before putting the site online.
- This project is an educational training app, not a real hacking toolkit.
