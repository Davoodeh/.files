/* -*- eval: (format-all-mode 0); -*- */
/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 0;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const unsigned int systraypinning = 0;   /* 0: sloppy systray follows selected monitor, >0: pin systray to monitor X */
static const unsigned int systrayspacing = 2;   /* systray spacing */
static const int systraypinningfailfirst = 1;   /* 1: if pinning fails, display systray on the first monitor, False: display systray on the last monitor*/
static const int showsystray        = 1;        /* 0 means no systray */
static const unsigned int gappih    = 0;        /* horiz inner gap between windows */
static const unsigned int gappiv    = 0;        /* vert inner gap between windows */
static const unsigned int gappoh    = 0;        /* horiz outer gap between windows and screen edge */
static const unsigned int gappov    = 0;        /* vert outer gap between windows and screen edge */
static const int swallowfloating    = 0;        /* 1 means swallow floating windows by default */
static const int smartgaps          = 0;        /* 1 means no outer gap when there is only one window */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char *fonts[]          = { "monospace:size=10" };
static char dmenufont[]             = "monospace:size=10";
static char normbgcolor[]           = "#222222";
static char normbordercolor[]       = "#444444";
static char normfgcolor[]           = "#bbbbbb";
static char selfgcolor[]            = "#eeeeee";
static char selbordercolor[]        = "#770000";
static char selbgcolor[]            = "#151515";
static char *colors[][3] = {
       /*               fg           bg           border   */
       [SchemeNorm] = { normfgcolor, normbgcolor, normbordercolor },
       [SchemeSel]  = { selfgcolor,  selbgcolor,  selbordercolor  },
};

typedef struct {
	const char *name;
	const void *cmd;
} Sp;
const char *spcmd1[] = {"st", "-n", "spterm", "-g", "120x34", NULL };
const char *spcmd2[] = {"st", "-n", "spcalc", "-g", "70x28", "-e", "python", NULL };
static Sp scratchpads[] = {
	/* name          cmd  */
	{"spterm",      spcmd1},
	{"spcalc",      spcmd2},
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	*/
	/* class    instance      title       	 tags mask    isfloating   isterminal  noswallow  monitor */
	{ "Gimp",     NULL,       NULL,       	    1 << 8,       0,           0,         0,        -1 },
	{ "St",       NULL,       NULL,       	    0,            0,           1,         0,        -1 },
	{ NULL,       NULL,       "Event Tester",   0,            0,           0,         1,        -1 },
	{ NULL,      "spterm",    NULL,       	    SPTAG(0),     1,           1,         0,        -1 },
	{ NULL,      "spcalc",    NULL,       	    SPTAG(1),     1,           1,         0,        -1 },
};

/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */
#define FORCE_VSPLIT 1  /* nrowgrid layout: force two clients to always split vertically */
#include "vanitygaps.c"
static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",	tile },			/* Default: Master on left, slaves on right */
	{ "TTT",	bstack },		/* Master on top, slaves on bottom */

	{ "[@]",	spiral },		/* Fibonacci spiral */
	{ "[\\]",	dwindle },		/* Decreasing in size right and leftward */

	{ "H[]",	deck },			/* Master on left, slaves in monocle-like mode on right */
	{ "[M]",	monocle },		/* All windows on top of eachother */

	{ "|M|",	centeredmaster },		/* Master in middle, slaves on sides */
	{ ">M>",	centeredfloatingmaster },	/* Same but master floats */

	{ "><>",	NULL },			/* no layout function means floating behavior */
	{ NULL,		NULL },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },
#define STACKKEYS(MOD,ACTION) \
	{ MOD, /*j*/   44,     ACTION##stack, {.i = INC(+1) } }, \
	{ MOD, /*k*/   45,     ACTION##stack, {.i = INC(-1) } }, \
	{ MOD, /*v*/   45,     ACTION##stack, {.i = 0 } }, \
	/* { MOD, /\*`*\/   49,     ACTION##stack, {.i = PREVSEL } }, \ */
	/* { MOD, /\*a*\/   38,     ACTION##stack, {.i = 1 } }, \ */
	/* { MOD, /\*z*\/   52,     ACTION##stack, {.i = 2 } }, \ */
	/* { MOD, /\*x*\/   54,     ACTION##stack, {.i = -1 } }, */

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", normbgcolor, "-nf", normfgcolor, "-sb", selbgcolor, "-sf", selfgcolor, NULL };
static const char *termcmd[]  = { "terminal", NULL };

#include <X11/XF86keysym.h>
#include "shiftview.c"
static Key keys[] = {
	/* modifier             keycode          function     arg */
	STACKKEYS(MODKEY,                          focus)
	STACKKEYS(MODKEY|ShiftMask,                push)
	{ ControlMask,          /*ESC*/  9,       spawn,      SHCMD("tm") },
	{ MODKEY,               /*`*/   49,       spawn,      SHCMD("dmenuunicode") },
	TAGKEYS(                /*1*/   10,       0)
	TAGKEYS(                /*2*/   11,       1)
	TAGKEYS(                /*3*/   12,       2)
	TAGKEYS(                /*4*/   13,       3)
	TAGKEYS(                /*5*/   14,       4)
	TAGKEYS(                /*6*/   15,       5)
	TAGKEYS(                /*7*/   16,       6)
	TAGKEYS(                /*8*/   17,       7)
	TAGKEYS(                /*9*/   18,       8)
	{ MODKEY,               /*0*/   19,       view,       {.ui = ~0 } },
	{ MODKEY|ShiftMask,     /*0*/   19,       tag,        {.ui = ~0 } },
	{ MODKEY,               /*-*/   20,       spawn,      SHCMD("pamixer --allow-boost -d ${SOUNDL:-5}; kill -44 $(pidof dwmblocks)") },
	{ MODKEY|ShiftMask,     /*-*/   20,       spawn,      SHCMD("pamixer --allow-boost -d $((${SOUNDL:-5}*3)); kill -44 $(pidof dwmblocks)") },
	{ MODKEY,               /*=*/   21,       spawn,      SHCMD("pamixer --allow-boost -i ${SOUNDL:-5}; kill -44 $(pidof dwmblocks)") },
	{ MODKEY|ShiftMask,     /*=*/   21,       spawn,      SHCMD("pamixer --allow-boost -i $((${SOUNDL:-5}*3)); kill -44 $(pidof dwmblocks)") },
	{ MODKEY,               /*<--*/ 22,       spawn,      SHCMD("sysact") },
	{ MODKEY|ShiftMask,     /*<--*/ 22,       spawn,      SHCMD("sysact") },

	{ MODKEY,               /*TAB*/ 23,       view,       {0} },
	{ MODKEY,               /*q*/   24,       killclient, {0} },
	{ MODKEY,               /*w*/   25,       spawn,      SHCMD("browser") },
	{ MODKEY|ShiftMask,     /*w*/   25,       spawn,      SHCMD("terminal -e sudo nmtui") },
	{ MODKEY,               /*e*/   26,       spawn,      SHCMD("fm") },
	{ MODKEY,               /*r*/   27,       spawn,      SHCMD("terminal -e neomutt ; pkill -RTMIN+12 dwmblocks; rmdir ~/.abook") },
	{ MODKEY|ShiftMask,     /*r*/   27,       spawn,      SHCMD("terminal -e abook -C ~/.config/abook/abookrc --datafile ~/.config/abook/addressbook") },
	{ MODKEY,               /*t*/   28,       setlayout,  {.v = &layouts[0]} }, /* tile */
	{ MODKEY|ShiftMask,     /*t*/   28,       setlayout,  {.v = &layouts[1]} }, /* bstack */
	{ MODKEY,               /*y*/   29,       setlayout,  {.v = &layouts[2]} }, /* spiral */
	{ MODKEY|ShiftMask,     /*y*/   29,       setlayout,  {.v = &layouts[3]} }, /* dwindle */
	{ MODKEY,               /*u*/   30,       setlayout,  {.v = &layouts[4]} }, /* deck */
	{ MODKEY|ShiftMask,     /*u*/   30,       setlayout,  {.v = &layouts[5]} }, /* monocle */
	{ MODKEY,               /*i*/   31,       setlayout,  {.v = &layouts[6]} }, /* centeredmaster */
	{ MODKEY|ShiftMask,     /*i*/   31,       setlayout,  {.v = &layouts[7]} }, /* centeredfloatingmaster */
	{ MODKEY,               /*o*/   32,       incnmaster, {.i = +1 } },
	{ MODKEY|ShiftMask,     /*o*/   32,       incnmaster, {.i = -1 } },
	{ MODKEY,               /*p*/   33,       spawn,      SHCMD("mpc toggle") },
	{ MODKEY|ShiftMask,     /*p*/   33,       spawn,      SHCMD("mpc pause ; pauseallmpv") },
	{ MODKEY,               /*[*/   34,       spawn,      SHCMD("mpc seek -10") },
	{ MODKEY|ShiftMask,     /*[*/   34,       spawn,      SHCMD("mpc seek -60") },
	{ MODKEY,               /*]*/   35,       spawn,      SHCMD("mpc seek +10") },
	{ MODKEY|ShiftMask,     /*]*/   35,       spawn,      SHCMD("mpc seek +60") },
	{ MODKEY,               /*\*/   51,       view,       {0} },

	{ MODKEY,               /*a*/   38,       togglegaps, {0} },
	{ MODKEY|ShiftMask,     /*a*/   38,       defaultgaps,{0} },
	{ MODKEY,               /*s*/   39,       togglesticky,  {0} },
	{ MODKEY,               /*d*/   40,       spawn,      {.v = dmenucmd } },
	{ MODKEY,               /*f*/   41,       togglefullscr, {0} },
	{ MODKEY|ShiftMask,     /*f*/   41,       setlayout,  {.v = &layouts[8]} },
	{ MODKEY,               /*g*/   42,       shiftview,  { .i = -1 } },
	{ MODKEY|ShiftMask,     /*g*/   42,       shifttag,   { .i = -1 } },
	{ MODKEY,               /*h*/   43,       setmfact,   {.f = -0.05} },
	{ MODKEY|ShiftMask,     /*h*/   43,       setmfact,   {.f = -0.05} },
	/* J and K are automatically bound above in STACKEYS */
	{ MODKEY,               /*l*/   46,       setmfact,   {.f = +0.05} },
	{ MODKEY|ShiftMask,     /*l*/   46,       setmfact,   {.f = +0.05} },
	{ MODKEY,               /*;*/   47,       shiftview,  { .i = 1 } },
	{ MODKEY|ShiftMask,     /*;*/   47,       shifttag,   { .i = 1 } },
	{ MODKEY,               /*'*/   48,       togglescratch, {.ui = 1} },
	{ MODKEY,               /*RET*/ 36,       spawn,      {.v = termcmd } },
	{ MODKEY|ShiftMask,     /*RET*/ 36,       togglescratch, {.ui = 0} },

	{ MODKEY,               /*z*/   52,       incrgaps,   {.i = +3 } },
	{ MODKEY,               /*x*/   53,       incrgaps,   {.i = -3 } },
	/* V is automatically bound above in STACKKEYS */
	{ MODKEY,               /*b*/   56,       togglebar,  {0} },
	{ MODKEY,               /*n*/   57,       spawn,      SHCMD("editor") },
	{ MODKEY|ShiftMask,     /*n*/   57,       spawn,      SHCMD("terminal -e newsboat; pkill -RTMIN+6 dwmblocks") },
	{ MODKEY,               /*m*/   58,       spawn,      SHCMD("terminal -e ncmpcpp") },
	{ MODKEY|ShiftMask,     /*m*/   58,       spawn,      SHCMD("pamixer -t; kill -44 $(pidof dwmblocks)") },
	{ MODKEY,               /*,*/   59,       spawn,      SHCMD("mpc prev") },
	{ MODKEY|ShiftMask,     /*,*/   59,       spawn,      SHCMD("mpc seek 0%") },
	{ MODKEY,               /*.*/   60,       spawn,      SHCMD("mpc next") },
	{ MODKEY|ShiftMask,     /*.*/   60,       spawn,      SHCMD("mpc repeat") },
	{ MODKEY,               /*/*/   61,       spawn,      {.v = termcmd } },

	{ MODKEY,               /*<-*/ 113,       focusmon,   {.i = -1 } },
	{ MODKEY|ShiftMask,     /*<-*/ 113,       tagmon,     {.i = -1 } },
	{ MODKEY,               /*->*/ 114,       focusmon,   {.i = +1 } },
	{ MODKEY|ShiftMask,     /*->*/ 114,       tagmon,     {.i = +1 } },

	{ MODKEY,               /*INS*/118,       spawn,      SHCMD("notify-send \"$(ico cb) Clipboard contents:\" \"$(xclip -o -selection clipboard)\"") },

	{ MODKEY,               /*F1*/  67,       spawn,      SHCMD("groff -mom /usr/local/share/dwm/larbs.mom -Tpdf | zathura -") },
	{ MODKEY,               /*F2*/  68,       spawn,      SHCMD("tutorialvids") },
	{ MODKEY,               /*F3*/  69,       spawn,      SHCMD("displayselect") },
	{ MODKEY,               /*F4*/  70,       spawn,      SHCMD("sound; kill -44 $(pidof dwmblocks)") },
	{ Mod1Mask,             /*F4*/  70,       killclient, {0} },
	{ MODKEY,               /*F5*/  71,       spawn,      SHCMD("killall screenkey || screenkey &") },
	{ MODKEY,               /*F6*/  72,       spawn,      SHCMD("torwrap") },
	{ MODKEY,               /*F7*/  73,       spawn,      SHCMD("td-toggle") },
	{ MODKEY,               /*F8*/  74,       spawn,      SHCMD("mailsync") },
	{ MODKEY,               /*F9*/  75,       spawn,      SHCMD("dmenumount") },
	{ MODKEY,               /*F10*/ 76,       spawn,      SHCMD("dmenuumount") },
	{ MODKEY,               /*F11*/ 95,       spawn,      SHCMD("mpv --no-cache --no-osc --no-input-default-bindings --input-conf=/dev/null --title=webcam $(ls /dev/video[0,2,4,6,8] | tail -n 1)") },
	{ MODKEY,               /*F12*/ 96,       xrdb,       { .v = NULL } }, /* Do not change, is bound to be pressed upon wallpaper change in `setbg`, refreshes colors */
	{ MODKEY,               /*SPC*/ 65,       spawn,      SHCMD("kblayout cycle $(cat $HOME/.config/vars/kblayouts)") },
	{ Mod1Mask,             /*SPC*/ 65,       zoom,       {0} },
	{ MODKEY|ShiftMask,     /*SPC*/ 65,       togglefloating, {0} },

	{ 0,                    /*PRT*/107,       spawn,      SHCMD("maimpick -i") },
	{ ShiftMask,            /*PRT*/107,       spawn,      SHCMD("maimpick") },
	{ MODKEY,               /*PRT*/107,       spawn,      SHCMD("dmenurecord") },
	{ MODKEY|ShiftMask,     /*PRT*/107,       spawn,      SHCMD("dmenurecord kill") },

	{ 0, /*XF86AudioMute*/         121,       spawn,      SHCMD("pamixer -t; kill -44 $(pidof dwmblocks)") },
	{ 0, /*XF86AudioLowerVolume*/  122,       spawn,      SHCMD("pamixer --allow-boost -d 3; kill -44 $(pidof dwmblocks)") },
	{ 0, /*XF86AudioRaiseVolume*/  123,       spawn,      SHCMD("pamixer --allow-boost -i 3; kill -44 $(pidof dwmblocks)") },
	{ 0, /*XF86AudioNext*/         171,       spawn,      SHCMD("mpc next") },
	{ 0, /*XF86AudioPrev*/         173,       spawn,      SHCMD("mpc prev") },
	{ 0, /*XF86AudioStop*/         174,       spawn,      SHCMD("mpc stop") },
	{ 0, /*XF86AudioRewind*/       176,       spawn,      SHCMD("mpc seek -10") },
	{ 0, /*XF86AudioMicMute*/      198,       spawn,      SHCMD("pactl set-source-mute @DEFAULT_SOURCE@ toggle") },
	{ 0, /*XF86AudioPlay*/         172,       spawn,      SHCMD("mpc toggle") },
	{ 0, /*XF86AudioPause*/        209,       spawn,      SHCMD("mpc pause") },
	{ 0, /*XF86AudioForward*/      216,       spawn,      SHCMD("mpc seek +10") },
	{ 0, /*XF86AudioMedia*/        234,       spawn,      SHCMD("terminal -e ncmpcpp") },
	/* { 0, /\*XF86Battery*\/           244,       spawn,      SHCMD("") }, */
	{ 0, /*XF86Calculator*/        148,       spawn,      SHCMD("calculator") },
	{ 0, /*XF86DOS*/               159,       spawn,      SHCMD("terminal") },
	{ 0, /*XF86Launch1*/           156,       spawn,      SHCMD("xset dpms force off") },
	{ 0, /*XF86Mail*/              163,       spawn,      SHCMD("terminal -e neomutt ; pkill -RTMIN+12 dwmblocks") },
	{ 0, /*XF86MonBrightnessDown*/ 232,       spawn,      SHCMD("sudo light -U ${LIGHTL:-5}") },
	{ 0, /*XF86MonBrightnessUp*/   233,       spawn,      SHCMD("sudo light -A ${LIGHTL:-5}") },
	{ 0, /*XF86MyComputer*/        165,       spawn,      SHCMD("fm /") },
	{ 0, /*XF86PowerOff*/          124,       spawn,      SHCMD("sysact") },
	{ 0, /*XF86ScreenSaver*/       160,       spawn,      SHCMD("lock & xset dpms force off; mpc pause; pauseallmpv") },
	/* { 0, /\*XF86Sleep*\/             150,       spawn,      SHCMD("sudo -A zzz") }, */
	{ 0, /*XF86TaskPane*/          162,       spawn,      SHCMD("tm") },
	{ 0, /*XF86TouchpadToggle*/    199,       spawn,      SHCMD("toggletouchpad") },
	{ 0, /*XF86TouchpadOn*/        200,       spawn,      SHCMD("toggletouchpad 0") },
	{ 0, /*XF86TouchpadOff*/       201,       spawn,      SHCMD("toggletouchpad 1") },
	{ 0, /*XF86WWW*/               158,       spawn,      SHCMD("browser") },

	/* { MODKEY,               /\*o*\/   32,      incrihgaps, {.i = -1 } }, */
	/* { MODKEY|ControlMask,   /\*o*\/   32,      incrivgaps, {.i = -1 } }, */
	/* { MODKEY|Mod4Mask,      /\*o*\/   32,      incrohgaps, {.i = -1 } }, */
	/* { MODKEY|ShiftMask,     /\*o*\/   32,      incrovgaps, {.i = -1 } }, */
	/* { MODKEY,               /\*y*\/   46,      incrihgaps, {.i = +1 } }, */
	/* { MODKEY|ControlMask,   /\*y*\/   46,      incrivgaps, {.i = +1 } }, */
	/* { MODKEY|Mod4Mask,      /\*y*\/   46,      incrohgaps, {.i = +1 } }, */
	/* { MODKEY|ShiftMask,     /\*y*\/   46,      incrovgaps, {.i = +1 } }, */
	/* { MODKEY|Mod4Mask,      /\*h*\/   43,      incrgaps,   {.i = +1 } }, */
	/* { MODKEY|Mod4Mask,      /\*l*\/   46,      incrgaps,   {.i = -1 } }, */
	/* { MODKEY|Mod4Mask|ShiftMask,   /\*h*\/ 43,  incrogaps,  {.i = +1 } }, */
	/* { MODKEY|Mod4Mask|ControlMask, /\*h*\/ 43,  incrigaps,  {.i = +1 } }, */
	/* { MODKEY|Mod4Mask|ShiftMask,   /\*l*\/ 46,  incrogaps,  {.i = -1 } }, */
	/* { MODKEY|Mod4Mask|ControlMask, /\*l*\/ 46,  incrigaps,  {.i = -1 } }, */
	/* { MODKEY|Mod4Mask|ShiftMask,   /\*0*\/ 19,  defaultgaps,   {0} }, */

};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
#ifndef __OpenBSD__
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button1,        sigdwmblocks,   {.i = 1} },
	{ ClkStatusText,        0,              Button2,        sigdwmblocks,   {.i = 2} },
	{ ClkStatusText,        0,              Button3,        sigdwmblocks,   {.i = 3} },
	{ ClkStatusText,        0,              Button4,        sigdwmblocks,   {.i = 4} },
	{ ClkStatusText,        0,              Button5,        sigdwmblocks,   {.i = 5} },
	{ ClkStatusText,        ShiftMask,      Button1,        sigdwmblocks,   {.i = 6} },
#endif
	{ ClkStatusText,        ShiftMask,      Button3,        spawn,          SHCMD("editor ~/.local/src/dwmblocks/config.h") },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        defaultgaps,    {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkClientWin,         MODKEY,         Button4,        incrgaps,       {.i = +1} },
	{ ClkClientWin,         MODKEY,         Button5,        incrgaps,       {.i = -1} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
	{ ClkTagBar,            0,              Button4,        shiftview,      {.i = -1} },
	{ ClkTagBar,            0,              Button5,        shiftview,      {.i = 1} },
	{ ClkRootWin,           0,              Button2,        togglebar,      {0} },
};
