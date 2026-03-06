let css =
  {|*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
@supports(corner-shape:squircle){*{corner-shape:squircle}}
html{
  --bg:#0D0B0F;--bg2:#13111A;--bg3:#1A1822;
  --brass:#C9A84C;--brass-lt:#E2C97E;--brass-dk:#8B7332;
  --gold:#F0D878;--teal:#2E8B7A;
  --txt:#E8E2D6;--txt2:#9C978A;--txt3:#6B6660;
  --border:#8B733240;--qed-gold:#B8860B;
}
body{
  min-height:100vh;
  background-color:var(--bg);
  background-image:radial-gradient(circle,rgba(201,168,76,0.18) 1.5px,transparent 1.5px);
  background-size:2.5rem 2.5rem;
  color:var(--txt);
  font-family:"Crimson Pro",Georgia,serif;
  font-size:1.08rem;line-height:1.72;
  display:flex;align-items:center;justify-content:center;
  -webkit-font-smoothing:antialiased;
}
body::after{
  content:'';position:fixed;inset:0;pointer-events:none;z-index:9999;opacity:0.035;
  background-image:url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='300' height='300'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.65' numOctaves='3' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='300' height='300' filter='url(%23n)'/%3E%3C/svg%3E");
  background-repeat:repeat;background-size:200px 200px;mix-blend-mode:overlay;
}
.card{
  position:relative;max-width:420px;width:90vw;padding:2.5rem 2rem;
  background:var(--bg2);border-radius:16px;border:1px solid var(--border);
  text-align:center;
}
.card::before,.card::after{
  content:"";position:absolute;width:16px;height:16px;pointer-events:none;
}
.card::before{top:10px;left:10px;border-top:1px solid var(--brass);border-left:1px solid var(--brass)}
.card::after{bottom:10px;right:10px;border-bottom:1px solid var(--brass);border-right:1px solid var(--brass)}
.corner-tr,.corner-bl{position:absolute;width:16px;height:16px;pointer-events:none}
.corner-tr{top:10px;right:10px;border-top:1px solid var(--brass);border-right:1px solid var(--brass)}
.corner-bl{bottom:10px;left:10px;border-bottom:1px solid var(--brass);border-left:1px solid var(--brass)}
h1{
  font-family:"Cormorant Garamond",Georgia,serif;font-size:1.6rem;font-weight:700;
  margin-bottom:0.75rem;padding-bottom:0.75rem;position:relative;
  border-bottom:2px solid var(--brass);
  background:linear-gradient(90deg,var(--brass-dk),var(--brass) 40%,var(--gold) 60%,var(--brass) 80%,var(--brass-dk));
  background-clip:text;-webkit-background-clip:text;color:transparent;
}
h1::after{
  content:"\2699";position:absolute;bottom:-0.6em;left:50%;
  transform:translateX(-50%);font-size:0.85rem;color:var(--brass);
  background:var(--bg2);padding:0 0.5rem;line-height:1;
}
.label{font-family:"EB Garamond",Georgia,serif;font-variant:small-caps;
  letter-spacing:0.06em;font-size:0.95rem;margin-bottom:1rem;display:block}
.label-ok{color:var(--teal)}
.label-error{color:var(--brass)}
p{color:var(--txt2);max-width:40ch;margin:0 auto 1rem}
.qed{color:var(--qed-gold);font-size:0.7rem;opacity:0.7;margin-top:1.25rem}
a{color:var(--brass);text-decoration:none}
a:hover{color:var(--brass-lt)}|}

let render ~title ~extra_css ~body_html =
  let all_css = if extra_css = "" then css else css ^ "\n" ^ extra_css in
  Printf.sprintf
    {|<!DOCTYPE html>
<html data-theme="dark">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>%s — clawq</title>
<style>
%s
</style>
</head>
<body>
<div class="card">
  <span class="corner-tr"></span>
  <span class="corner-bl"></span>
  %s
</div>
</body>
</html>|}
    title all_css body_html
