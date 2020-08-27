/* See LICENSE file for copyright and license details. */

/* appearance */
static const char font[]        = "monospace:size=10";
static const char* normbgcolor  = "#222222";
static const char* normfgcolor  = "#cccccc";
static const char* selbgcolor   = "#555555";
static const char* selfgcolor   = "#ffffff";
static const char* urgbgcolor   = "#111111";
static const char* urgfgcolor   = "#cc0000";
static const char before[]      = "<";
static const char after[]       = ">";
static const char titletrim[]   = "...";
static const int  tabwidth      = 200;
static const Bool foreground    = True;
static       Bool urgentswitch  = False;

/*
 * Where to place a new tab when it is opened. When npisrelative is True,
 * then the current position is changed + newposition. If npisrelative
 * is False, then newposition is an absolute position.
 */
static int  newposition   = 0;
static Bool npisrelative  = False;

#define SETPROP(p) { \
        .v = (char *[]){ "/bin/sh", "-c", \
                "prop=\"`xwininfo -children -id $1 | grep '^     0x' |" \
                "sed -e's@^ *\\(0x[0-9a-f]*\\) \"\\([^\"]*\\)\".*@\\1 \\2@' |" \
                "xargs -0 printf %b | dmenu -l 10 -w $1`\" &&" \
                "xprop -id $1 -f $0 8s -set $0 \"$prop\"", \
                p, winid, NULL \
        } \
}

#define MODKEY ControlMask
static Key keys[] = {
	/* modifier             key            function     argument */
	{ MODKEY|Mod1Mask,      /*RET*/ 36,    focusonce,   { 0 } },
	{ MODKEY|Mod1Mask,      /*RET*/ 36,    spawn,       { 0 } },

	{ MODKEY|Mod1Mask,      /*l*/   46,    rotate,      { .i = +1 } },
	{ MODKEY|Mod1Mask,      /*h*/   43,    rotate,      { .i = -1 } },
	{ MODKEY|Mod1Mask,      /*j*/   44,    movetab,     { .i = -1 } },
	{ MODKEY|Mod1Mask,      /*k*/   45,    movetab,     { .i = +1 } },
	{ MODKEY,               /*TAB*/ 23,    rotate,      { .i = 0 } },

	{ MODKEY,               /*`*/   49,    spawn,       SETPROP("_TABBED_SELECT_TAB") },
	{ MODKEY,               /*1*/   10,    move,        { .i = 0 } },
	{ MODKEY,               /*2*/   11,    move,        { .i = 1 } },
	{ MODKEY,               /*3*/   12,    move,        { .i = 2 } },
	{ MODKEY,               /*4*/   13,    move,        { .i = 3 } },
	{ MODKEY,               /*5*/   14,    move,        { .i = 4 } },
	{ MODKEY,               /*6*/   15,    move,        { .i = 5 } },
	{ MODKEY,               /*7*/   16,    move,        { .i = 6 } },
	{ MODKEY,               /*8*/   17,    move,        { .i = 7 } },
	{ MODKEY,               /*9*/   18,    move,        { .i = 8 } },
	{ MODKEY,               /*0*/   19,    move,        { .i = 9 } },

	{ MODKEY,               /*q*/   24,    killclient,  { 0 } },

	{ MODKEY,               /*u*/   30,    focusurgent, { 0 } },
	{ MODKEY|Mod1Mask,      /*u*/   30,    toggle,      { .v = (void*) &urgentswitch } },

	{ 0,                    /*F11*/ 95,    fullscreen,  { 0 } },

	{ MODKEY,               /*Alt*/ 64,    showbar,    { .i = 1 } },
	{ Mod1Mask,             /*CTR*/ 37,    showbar,    { .i = 1 } },
};

static Key keyreleases[] = {
	/* modifier              key           function     argument */
	{ MODKEY|Mod1Mask,      /*Alt*/ 64,    showbar,     { .i = 0 } },
	{ MODKEY|Mod1Mask,      /*CTR*/ 64,    showbar,     { .i = 0 } },
};
