import { expect, test } from "bun:test";
import { ansiToHtml } from "./ansi";

test("renders plain text without ansi wrappers", () => {
  expect(ansiToHtml("hello")).toBe("hello");
});

test("renders bold ansi segments", () => {
  expect(ansiToHtml("\u001b[1mhi\u001b[0m")).toContain("font-weight:700");
});

test("renders color ansi segments", () => {
  expect(ansiToHtml("\u001b[32mok\u001b[0m")).toContain("color:#2f7d4b");
});
