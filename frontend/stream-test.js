const ac = new AudioContext();

const frameCount = ac.sampleRate * 2.0; // two seconds
const ab = ac.createBuffer(2, frameCount, ac.sampleRate);

for (let c = 0; c < 2; c++) {
  const buf = ab.getChannelData(c);
  for (let i = 0; i < frameCount; i++) {
    buf[i] = 0.1 * (Math.random() * 2 - 1);
  }
}

const src = ac.createBufferSource();
src.buffer = ab;
src.connect(ac.destination);
src.start();
