(() => {
  const STORE_KEY = 'cyberquest_sound_enabled';
  let soundEnabled = localStorage.getItem(STORE_KEY) !== 'false';
  let audioContext;
  const soundButton = document.getElementById('soundToggle');
  const syncSoundButton = () => { if (soundButton) { soundButton.textContent = soundEnabled ? '🔊' : '🔇'; soundButton.title = soundEnabled ? 'Turn sound off' : 'Turn sound on'; } };
  syncSoundButton();
  if (soundButton) soundButton.addEventListener('click', () => { soundEnabled = !soundEnabled; localStorage.setItem(STORE_KEY, String(soundEnabled)); syncSoundButton(); if (soundEnabled) beep(660, 0.07); });

  function beep(freq = 520, duration = 0.1) {
    if (!soundEnabled) return;
    try {
      audioContext = audioContext || new (window.AudioContext || window.webkitAudioContext)();
      if (audioContext.state === 'suspended') audioContext.resume();
      const oscillator = audioContext.createOscillator();
      const gain = audioContext.createGain();
      oscillator.type = 'triangle'; oscillator.frequency.value = freq;
      gain.gain.setValueAtTime(0.0001, audioContext.currentTime);
      gain.gain.exponentialRampToValueAtTime(0.08, audioContext.currentTime + 0.015);
      gain.gain.exponentialRampToValueAtTime(0.0001, audioContext.currentTime + duration);
      oscillator.connect(gain); gain.connect(audioContext.destination); oscillator.start(); oscillator.stop(audioContext.currentTime + duration + 0.02);
    } catch (_) { /* Sound is optional; the game still works without Web Audio. */ }
  }

  function confetti() {
    const layer = document.createElement('div'); layer.className = 'confetti-layer'; document.body.appendChild(layer);
    const symbols = ['✦', '◆', '●', '+', '★'];
    for (let i = 0; i < 65; i++) {
      const piece = document.createElement('span'); piece.className = 'confetti-piece'; piece.textContent = symbols[i % symbols.length];
      piece.style.left = `${Math.random() * 100}%`; piece.style.animationDelay = `${Math.random() * 0.65}s`; piece.style.animationDuration = `${1.6 + Math.random() * 1.7}s`; piece.style.setProperty('--drift', `${Math.round((Math.random() - 0.5) * 220)}px`); layer.appendChild(piece);
    }
    window.setTimeout(() => layer.remove(), 4000);
  }

  function toast(title, subtitle) {
    const layer = document.getElementById('toastLayer'); if (!layer) return;
    const el = document.createElement('div'); el.className = 'game-toast';
    const strong = document.createElement('strong'); strong.textContent = title;
    const small = document.createElement('span'); small.textContent = subtitle || '';
    el.append(strong, small); layer.appendChild(el); window.setTimeout(() => el.remove(), 4200);
  }

  function celebrate(data = {}) {
    confetti(); beep(660, 0.12); window.setTimeout(() => beep(880, 0.14), 130); window.setTimeout(() => beep(1100, 0.18), 260);
    const overlay = document.createElement('div'); overlay.className = 'celebration-overlay';
    const card = document.createElement('div'); card.className = 'celebration-card';
    const close = document.createElement('button'); close.className = 'celebration-close'; close.type = 'button'; close.textContent = '×'; close.setAttribute('aria-label', 'Close celebration');
    const icon = document.createElement('div'); icon.className = 'celebration-icon'; icon.textContent = '🏆';
    const title = document.createElement('h2'); title.textContent = data.title || 'LEVEL UP!';
    const subtitle = document.createElement('p'); subtitle.textContent = data.subtitle || 'Great work, defender.';
    const xp = document.createElement('div'); xp.className = 'celebration-xp'; xp.textContent = `+${Number(data.xp || 0)} XP`;
    card.append(close, icon, title, subtitle, xp); overlay.appendChild(card); document.body.appendChild(overlay);
    const dismiss = () => overlay.remove(); close.addEventListener('click', dismiss); overlay.addEventListener('click', e => { if (e.target === overlay) dismiss(); });
    window.setTimeout(dismiss, 6500); toast('XP ACQUIRED', `+${Number(data.xp || 0)} XP added to your run`);
  }

  document.querySelectorAll('.answer-option').forEach(option => option.addEventListener('change', () => beep(480, 0.045)));
  document.querySelectorAll('.btn').forEach(button => button.addEventListener('click', () => { if (!button.disabled) beep(540, 0.045); }));
  document.querySelectorAll('.xp-number').forEach(node => {
    const target = Number(node.dataset.xp || 0); if (!target) return;
    const start = performance.now(); const duration = 750;
    function tick(now) { const progress = Math.min(1, (now - start) / duration); node.textContent = Math.floor(target * (1 - Math.pow(1 - progress, 3))).toLocaleString(); if (progress < 1) requestAnimationFrame(tick); }
    node.textContent = '0'; requestAnimationFrame(tick);
  });
  window.CyberQuestGame = { celebrate, toast, beep };
})();
