import { readFileSync } from "fs";

function simulate(lines: string[]): [number, number, number] {
  let pos = 50;
  let zeroHits = 0;
  let crossings = 0;

  for (const raw of lines) {
    const line = raw.trim();
    if (!line) continue;
    const sign = line[0] === "R" ? 1 : -1;
    const mag = parseInt(line.slice(1), 10);

    let first = sign === 1 ? 100 - pos : pos;
    if (first === 0) first = 100;
    if (mag >= first) {
      crossings += 1 + Math.floor((mag - first) / 100);
    }

    pos = ((pos + sign * mag) % 100 + 100) % 100;
    if (pos === 0) zeroHits++;
  }
  return [zeroHits, crossings, pos];
}

const t0 = performance.now();
const lines = readFileSync("input.txt", "utf8").trim().split(/\r?\n/);
const [zeroHits, crossings, pos] = simulate(lines);
const elapsed = performance.now() - t0;
console.log(
  `zero_landings=${zeroHits} crossings=${crossings} final_pos=${pos} elapsed_ms=${elapsed.toFixed(
    3,
  )}`,
);
