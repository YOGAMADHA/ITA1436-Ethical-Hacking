import json
import os
import random
import sqlite3
from datetime import date, timedelta
from functools import wraps
from pathlib import Path

from flask import Flask, flash, g, redirect, render_template, request, session, url_for
from werkzeug.security import check_password_hash, generate_password_hash

BASE_DIR = Path(__file__).resolve().parent
INSTANCE_DIR = BASE_DIR / "instance"
INSTANCE_DIR.mkdir(exist_ok=True)

# Reuse a common existing database filename if this file is overlaid into an existing project.
_candidates = [BASE_DIR / "cyberquest.db", BASE_DIR / "database.db", INSTANCE_DIR / "cyberquest.db"]
DB_PATH = next((p for p in _candidates if p.exists()), INSTANCE_DIR / "cyberquest.db")

app = Flask(__name__)
app.secret_key = os.environ.get("CYBERQUEST_SECRET", "change-this-development-secret")
app.config["DATABASE"] = str(DB_PATH)


def get_db():
    if "db" not in g:
        g.db = sqlite3.connect(app.config["DATABASE"])
        g.db.row_factory = sqlite3.Row
        g.db.execute("PRAGMA foreign_keys = ON")
    return g.db


@app.teardown_appcontext
def close_db(_error=None):
    db = g.pop("db", None)
    if db is not None:
        db.close()


def column_names(db, table):
    return {row[1] for row in db.execute(f"PRAGMA table_info({table})").fetchall()}


def init_db():
    """Create missing tables and add reward columns without deleting existing records."""
    db = get_db()
    db.executescript("""
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT NOT NULL UNIQUE,
      password TEXT NOT NULL,
      xp INTEGER NOT NULL DEFAULT 0,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
    CREATE TABLE IF NOT EXISTS missions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      title TEXT NOT NULL,
      world TEXT NOT NULL DEFAULT 'Cyber Basics',
      topic TEXT NOT NULL DEFAULT 'Awareness',
      description TEXT NOT NULL DEFAULT '',
      question TEXT NOT NULL DEFAULT '',
      options TEXT NOT NULL DEFAULT '[]',
      answer TEXT NOT NULL DEFAULT '',
      explanation TEXT NOT NULL DEFAULT '',
      xp_reward INTEGER NOT NULL DEFAULT 50,
      world_order INTEGER NOT NULL DEFAULT 99,
      level_number INTEGER NOT NULL DEFAULT 0
    );
    CREATE TABLE IF NOT EXISTS completions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      mission_id INTEGER NOT NULL,
      score INTEGER NOT NULL DEFAULT 0,
      completed_at TEXT DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(user_id, mission_id),
      FOREIGN KEY(user_id) REFERENCES users(id),
      FOREIGN KEY(mission_id) REFERENCES missions(id)
    );
    CREATE TABLE IF NOT EXISTS user_achievements (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      achievement_key TEXT NOT NULL,
      earned_at TEXT DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(user_id, achievement_key),
      FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
    );
    CREATE TABLE IF NOT EXISTS user_gifts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      gift_key TEXT NOT NULL,
      gift_name TEXT NOT NULL,
      xp_bonus INTEGER NOT NULL DEFAULT 0,
      opened_at TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
    );
    CREATE TABLE IF NOT EXISTS mission_attempts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      mission_id INTEGER NOT NULL,
      is_correct INTEGER NOT NULL,
      attempted_at TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
    );
    """)

    # Add columns to an older users table only when missing.
    user_cols = column_names(db, "users")
    for col, definition in {
        "xp": "INTEGER NOT NULL DEFAULT 0",
        "current_streak": "INTEGER NOT NULL DEFAULT 0",
        "longest_streak": "INTEGER NOT NULL DEFAULT 0",
        "last_active_date": "TEXT",
        "avatar": "TEXT NOT NULL DEFAULT 'rookie'",
    }.items():
        if col not in user_cols:
            db.execute(f"ALTER TABLE users ADD COLUMN {col} {definition}")

    # Keep older completion tables compatible while adding timestamp when absent.
    completion_cols = column_names(db, "completions")
    if "completed_at" not in completion_cols:
        db.execute("ALTER TABLE completions ADD COLUMN completed_at TEXT")

    # Add the 40-mission campaign without deleting existing users, completions, or missions.
    mission_cols = column_names(db, "missions")
    for col, definition in {"world_order": "INTEGER NOT NULL DEFAULT 99", "level_number": "INTEGER NOT NULL DEFAULT 0"}.items():
        if col not in mission_cols:
            db.execute(f"ALTER TABLE missions ADD COLUMN {col} {definition}")

    seed_path = BASE_DIR / "missions_seed.json"
    if seed_path.exists():
        campaign = json.loads(seed_path.read_text(encoding="utf-8"))
        for item in campaign:
            existing = db.execute("SELECT id FROM missions WHERE title=?", (item["title"],)).fetchone()
            values = (item["world"], item["topic"], item["description"], item["question"], json.dumps(item["options"], ensure_ascii=False), item["answer"], item["explanation"], item["xp_reward"], item["world_order"], item["level_number"])
            if existing:
                db.execute("UPDATE missions SET world=?, topic=?, description=?, question=?, options=?, answer=?, explanation=?, xp_reward=?, world_order=?, level_number=? WHERE id=?", (*values, existing["id"]))
            else:
                db.execute("INSERT INTO missions (title, world, topic, description, question, options, answer, explanation, xp_reward, world_order, level_number) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", (item["title"], *values))

    # Place any older/custom missions after the structured 40-level campaign, preserving them.
    db.execute("UPDATE missions SET world_order=99, level_number=id WHERE world_order IS NULL OR world_order=0")
    db.commit()


with app.app_context():
    init_db()


def login_required(view):
    @wraps(view)
    def wrapped(*args, **kwargs):
        if "user_id" not in session:
            flash("Log in to save your XP and rewards.", "info")
            return redirect(url_for("login"))
        return view(*args, **kwargs)
    return wrapped


def current_user():
    if "user_id" not in session:
        return None
    return get_db().execute("SELECT * FROM users WHERE id = ?", (session["user_id"],)).fetchone()


def update_streak(user_id):
    db = get_db()
    user = db.execute("SELECT current_streak, longest_streak, last_active_date FROM users WHERE id=?", (user_id,)).fetchone()
    today = date.today()
    last = date.fromisoformat(user["last_active_date"]) if user["last_active_date"] else None
    if last == today:
        return int(user["current_streak"] or 0), False
    if last == today - timedelta(days=1):
        streak = int(user["current_streak"] or 0) + 1
    else:
        streak = 1
    longest = max(streak, int(user["longest_streak"] or 0))
    db.execute("UPDATE users SET current_streak=?, longest_streak=?, last_active_date=? WHERE id=?", (streak, longest, today.isoformat(), user_id))
    db.commit()
    return streak, True


ACHIEVEMENTS = {
    "first_mission": ("First Steps", "Complete your first mission", "🎯"),
    "three_missions": ("Mission Machine", "Complete 3 missions", "⚡"),
    "five_hundred_xp": ("XP Collector", "Earn 500 XP", "💎"),
    "streak_3": ("On a Roll", "Reach a 3-day streak", "🔥"),
    "streak_7": ("Weekly Warrior", "Reach a 7-day streak", "🏆"),
}


def grant_achievement(user_id, key, new_rewards):
    if key not in ACHIEVEMENTS:
        return
    db = get_db()
    cur = db.execute("INSERT OR IGNORE INTO user_achievements (user_id, achievement_key) VALUES (?, ?)", (user_id, key))
    if cur.rowcount:
        name, description, emoji = ACHIEVEMENTS[key]
        new_rewards.append({"type": "achievement", "name": name, "description": description, "emoji": emoji})


def check_achievements(user_id, streak=0):
    db = get_db()
    user = db.execute("SELECT xp FROM users WHERE id=?", (user_id,)).fetchone()
    count = db.execute("SELECT COUNT(*) FROM completions WHERE user_id=?", (user_id,)).fetchone()[0]
    rewards = []
    if count >= 1: grant_achievement(user_id, "first_mission", rewards)
    if count >= 3: grant_achievement(user_id, "three_missions", rewards)
    if user and user["xp"] >= 500: grant_achievement(user_id, "five_hundred_xp", rewards)
    if streak >= 3: grant_achievement(user_id, "streak_3", rewards)
    if streak >= 7: grant_achievement(user_id, "streak_7", rewards)
    db.commit()
    return rewards


@app.context_processor
def inject_globals():
    return {"logged_user": current_user(), "today_year": date.today().year}


@app.route("/")
def index():
    db = get_db()
    mission_count = db.execute("SELECT COUNT(*) FROM missions").fetchone()[0]
    player_count = db.execute("SELECT COUNT(*) FROM users").fetchone()[0]
    return render_template("index.html", mission_count=mission_count, player_count=player_count)


@app.route("/register", methods=["GET", "POST"])
def register():
    if request.method == "POST":
        username = request.form.get("username", "").strip()
        password = request.form.get("password", "")
        if len(username) < 3 or len(password) < 6:
            flash("Use a username with at least 3 characters and a password with at least 6.", "error")
        else:
            try:
                db = get_db()
                cur = db.execute("INSERT INTO users (username, password) VALUES (?, ?)", (username, generate_password_hash(password)))
                db.commit()
                session["user_id"] = cur.lastrowid
                streak, _ = update_streak(cur.lastrowid)
                flash("Player profile created. Your quest begins!", "success")
                return redirect(url_for("dashboard"))
            except sqlite3.IntegrityError:
                flash("That username is already taken.", "error")
    return render_template("auth.html", mode="register")


@app.route("/login", methods=["GET", "POST"])
def login():
    if request.method == "POST":
        username = request.form.get("username", "").strip()
        password = request.form.get("password", "")
        user = get_db().execute("SELECT * FROM users WHERE username=?", (username,)).fetchone()
        if user and (check_password_hash(user["password"], password) if user["password"].startswith("scrypt:") or user["password"].startswith("pbkdf2:") else user["password"] == password):
            session.clear()
            session["user_id"] = user["id"]
            update_streak(user["id"])
            return redirect(url_for("dashboard"))
        flash("Username or password is incorrect.", "error")
    return render_template("auth.html", mode="login")


@app.route("/logout")
def logout():
    session.clear()
    flash("You have logged out.", "info")
    return redirect(url_for("index"))


@app.route("/dashboard")
@login_required
def dashboard():
    db = get_db()
    user = current_user()
    missions = db.execute("SELECT m.*, CASE WHEN c.id IS NULL THEN 0 ELSE 1 END AS completed FROM missions m LEFT JOIN completions c ON c.mission_id=m.id AND c.user_id=? ORDER BY m.world_order, m.level_number, m.id", (user["id"],)).fetchall()
    completed_ids = {r[0] for r in db.execute("SELECT mission_id FROM completions WHERE user_id=?", (user["id"],)).fetchall()}
    first_uncompleted = next((m["id"] for m in missions if m["id"] not in completed_ids), None)
    mission_list = []
    unlocked = True
    for m in missions:
        d = dict(m)
        d["is_locked"] = (not d["completed"] and not unlocked)
        mission_list.append(d)
        if d["id"] == first_uncompleted:
            unlocked = False
    missions = mission_list
    completed_count = db.execute("SELECT COUNT(*) FROM completions WHERE user_id=?", (user["id"],)).fetchone()[0]
    total_missions = db.execute("SELECT COUNT(*) FROM missions").fetchone()[0]
    achievements = db.execute("SELECT achievement_key, earned_at FROM user_achievements WHERE user_id=? ORDER BY earned_at DESC", (user["id"],)).fetchall()
    gifts = db.execute("SELECT * FROM user_gifts WHERE user_id=? ORDER BY id DESC", (user["id"],)).fetchall()
    return render_template("dashboard.html", user=user, missions=missions, completed_count=completed_count, total_missions=total_missions, achievements=achievements, achievement_info=ACHIEVEMENTS, gifts=gifts)


@app.route("/worlds")
@login_required
def worlds():
    rows = get_db().execute("SELECT world, COUNT(*) AS mission_count, MIN(id) AS first_id, MIN(world_order) AS world_order FROM missions GROUP BY world ORDER BY MIN(world_order), MIN(level_number), MIN(id)").fetchall()
    return render_template("worlds.html", worlds=rows)


@app.route("/mission/<int:mission_id>", methods=["GET", "POST"])
@login_required
def mission(mission_id):
    db = get_db()
    user = current_user()
    mission_row = db.execute("SELECT * FROM missions WHERE id=?", (mission_id,)).fetchone()
    if mission_row is None:
        flash("Mission not found.", "error")
        return redirect(url_for("dashboard"))
    done = db.execute("SELECT id FROM completions WHERE user_id=? AND mission_id=?", (user["id"], mission_id)).fetchone()
    options = json.loads(mission_row["options"] or "[]")
    ordered_ids = [r[0] for r in db.execute("SELECT id FROM missions ORDER BY world_order, level_number, id").fetchall()]
    completed_ids = {r[0] for r in db.execute("SELECT mission_id FROM completions WHERE user_id=?", (user["id"],)).fetchall()}
    if not done and mission_id in ordered_ids:
        position = ordered_ids.index(mission_id)
        prior_incomplete = next((mid for mid in ordered_ids[:position] if mid not in completed_ids), None)
        if prior_incomplete is not None:
            flash("Level locked! Clear the earlier mission to unlock this challenge.", "info")
            return redirect(url_for("mission", mission_id=prior_incomplete))
    if request.method == "POST" and not done:
        selected = request.form.get("answer", "")
        correct = selected == mission_row["answer"]
        db.execute("INSERT INTO mission_attempts (user_id, mission_id, is_correct) VALUES (?, ?, ?)", (user["id"], mission_id, int(correct)))
        db.commit()
        if correct:
            reward = max(0, int(mission_row["xp_reward"] or 0))
            db.execute("INSERT OR IGNORE INTO completions (user_id, mission_id, score) VALUES (?, ?, ?)", (user["id"], mission_id, 100))
            db.execute("UPDATE users SET xp=COALESCE(xp,0)+? WHERE id=?", (reward, user["id"]))
            db.commit()
            streak, _ = update_streak(user["id"])
            rewards = check_achievements(user["id"], streak)
            # Mystery gift: randomly awarded, persistent, and shown only when newly earned.
            gift = None
            if random.random() < 0.30:
                gift_names = [("neon_cache", "Neon XP Cache", 25), ("pixel_crate", "Pixel Crate", 40), ("bonus_orb", "Bonus XP Orb", 60)]
                key, name, bonus = random.choice(gift_names)
                db.execute("INSERT INTO user_gifts (user_id, gift_key, gift_name, xp_bonus) VALUES (?, ?, ?, ?)", (user["id"], key, name, bonus))
                db.execute("UPDATE users SET xp=COALESCE(xp,0)+? WHERE id=?", (bonus, user["id"]))
                db.commit()
                gift = {"name": name, "xp_bonus": bonus}
            session["quest_result"] = {"correct": True, "xp": reward + (gift["xp_bonus"] if gift else 0), "rewards": rewards, "gift": gift, "message": "Mission cleared!"}
            return redirect(url_for("mission", mission_id=mission_id))
        session["quest_result"] = {"correct": False, "xp": 0, "rewards": [], "gift": None, "message": "Not quite! Review the explanation and try another mission."}
        return redirect(url_for("mission", mission_id=mission_id))
    result = session.pop("quest_result", None)
    return render_template("mission.html", mission=mission_row, options=options, completed=bool(done), result=result)


@app.route("/leaderboard")
def leaderboard():
    players = get_db().execute("SELECT id, username, COALESCE(xp,0) AS xp, avatar FROM users ORDER BY xp DESC, username ASC LIMIT 20").fetchall()
    return render_template("leaderboard.html", players=players)


@app.route("/profile", methods=["GET", "POST"])
@login_required
def profile():
    db = get_db()
    user = current_user()
    avatars = {"rookie": "🧑‍💻", "cybercat": "🐱", "robot": "🤖", "fox": "🦊", "dragon": "🐉", "alien": "👾"}
    completed = db.execute("SELECT COUNT(*) FROM completions WHERE user_id=?", (user["id"],)).fetchone()[0]
    earned = {r[0] for r in db.execute("SELECT achievement_key FROM user_achievements WHERE user_id=?", (user["id"],)).fetchall()}
    unlocked = ["rookie"]
    if completed >= 1: unlocked.append("cybercat")
    if completed >= 2: unlocked.append("robot")
    if completed >= 3: unlocked.append("fox")
    if user["xp"] >= 250: unlocked.append("dragon")
    if user["xp"] >= 500: unlocked.append("alien")
    if request.method == "POST":
        chosen = request.form.get("avatar", "rookie")
        if chosen in unlocked:
            db.execute("UPDATE users SET avatar=? WHERE id=?", (chosen, user["id"]))
            db.commit()
            flash("Avatar updated!", "success")
        else:
            flash("Complete more missions or earn XP to unlock that avatar.", "error")
        return redirect(url_for("profile"))
    user = current_user()
    achievements = db.execute("SELECT achievement_key, earned_at FROM user_achievements WHERE user_id=? ORDER BY earned_at DESC", (user["id"],)).fetchall()
    gifts = db.execute("SELECT * FROM user_gifts WHERE user_id=? ORDER BY id DESC", (user["id"],)).fetchall()
    return render_template("profile.html", user=user, avatars=avatars, unlocked=unlocked, achievements=achievements, achievement_info=ACHIEVEMENTS, gifts=gifts)


if __name__ == "__main__":
    app.run(debug=True)
