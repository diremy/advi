/***********************************************************************/
/*                                                                     */
/*                           Active dvi                                */
/*                                                                     */
/*            Roberto Di Cosmo                                        */
/*            projet Cristal, INRIA Rocquencourt                       */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* Inspired from mplayer */

#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#include "libgraph.h"

#include "grwm.h"

#ifdef HAVE_MOTIF

#include <X11/Xm/MwmUtil.h>

#else

/* bit definitions for MwmHints.flags */
#define MWM_HINTS_FUNCTIONS	(1L << 0)
#define MWM_HINTS_DECORATIONS	(1L << 1)
#define MWM_HINTS_INPUT_MODE	(1L << 2)
#define MWM_HINTS_STATUS	(1L << 3)

/* bit definitions for MwmHints.functions */
#define MWM_FUNC_ALL            (1L << 0)
#define MWM_FUNC_RESIZE         (1L << 1)
#define MWM_FUNC_MOVE           (1L << 2)
#define MWM_FUNC_MINIMIZE       (1L << 3)
#define MWM_FUNC_MAXIMIZE       (1L << 4)
#define MWM_FUNC_CLOSE          (1L << 5)

/* bit definitions for MwmHints.decorations */
#define MWM_DECOR_ALL		(1L << 0)
#define MWM_DECOR_BORDER	(1L << 1)
#define MWM_DECOR_RESIZEH	(1L << 2)
#define MWM_DECOR_TITLE		(1L << 3)
#define MWM_DECOR_MENU		(1L << 4)
#define MWM_DECOR_MINIMIZE	(1L << 5)
#define MWM_DECOR_MAXIMIZE	(1L << 6)

typedef struct
{
    unsigned long	flags;
    unsigned long	functions;
    unsigned long	decorations;
    long 	        inputMode;
    unsigned long	status;
} PropMotifWmHints;

#define PROP_MOTIF_WM_HINTS_ELEMENTS	5

#endif

static Atom XA_NET_SUPPORTED;
static Atom XA_NET_WM_STATE;
static Atom XA_NET_WM_STATE_FULLSCREEN;
static Atom XA_NET_WM_STATE_ABOVE;
static Atom XA_NET_WM_STATE_STAYS_ON_TOP;
static Atom XA_NET_WM_STATE_BELOW;
static Atom XA_NET_WM_PID;
static Atom XA_WIN_PROTOCOLS;
static Atom XA_WIN_LAYER;
static Atom XA_WIN_HINTS;
static Atom XA_BLACKBOX_PID;

void init_atoms(Display * mDisplay)
{
 XA_NET_SUPPORTED  = XInternAtom(mDisplay, "_NET_SUPPORTED", 0 ) ;
 XA_NET_WM_STATE  = XInternAtom(mDisplay, "_NET_WM_STATE", 0 ) ;
 XA_NET_WM_STATE_FULLSCREEN  = XInternAtom(mDisplay, "_NET_WM_STATE_FULLSCREEN", 0 ) ;
 XA_NET_WM_STATE_ABOVE  = XInternAtom(mDisplay, "_NET_WM_STATE_ABOVE", 0 ) ;
 XA_NET_WM_STATE_STAYS_ON_TOP  = XInternAtom(mDisplay, "_NET_WM_STATE_STAYS_ON_TOP", 0 ) ;
 XA_NET_WM_STATE_BELOW  = XInternAtom(mDisplay, "_NET_WM_STATE_BELOW", 0 ) ;
 XA_NET_WM_PID  = XInternAtom(mDisplay, "_NET_WM_PID", 0 ) ;
 XA_WIN_PROTOCOLS  = XInternAtom(mDisplay, "_WIN_PROTOCOLS", 0 ) ;
 XA_WIN_LAYER  = XInternAtom(mDisplay, "_WIN_LAYER", 0 ) ;
 XA_WIN_HINTS  = XInternAtom(mDisplay, "_WIN_HINTS", 0 ) ;
 XA_BLACKBOX_PID  = XInternAtom(mDisplay, "_BLACKBOX_PID", 0 ) ;
}

typedef struct
{
  long flags;
  long functions;
  long decorations;
  long input_mode;
  long state;
} MotifWmHints;

void x11_decoration( Display *dpy,Window w,int decorate, int fsmode )
{
  MotifWmHints MotifWmHints;
  Atom MotifHints;

  if(fsmode&1){
    XSetWindowAttributes attr;
    attr.override_redirect = (!decorate) ? True : False;
    XChangeWindowAttributes(dpy, w, CWOverrideRedirect, &attr);
  }

  if(fsmode&8){
    XSetTransientForHint (dpy, w, RootWindow(dpy,DefaultScreen(dpy)));
  }

 MotifHints=XInternAtom( dpy,"_MOTIF_WM_HINTS",0 );
 if ( MotifHints != None )
  {
   memset( &MotifWmHints,0,sizeof( MotifWmHints ) );
   MotifWmHints.flags=MWM_HINTS_FUNCTIONS | MWM_HINTS_DECORATIONS;
   if ( decorate )
    {
      MotifWmHints.functions=MWM_FUNC_MOVE | MWM_FUNC_CLOSE | MWM_FUNC_MINIMIZE | MWM_FUNC_MAXIMIZE | MWM_FUNC_RESIZE;
      decorate=MWM_DECOR_ALL;
    }
   
   MotifWmHints.decorations=decorate|((fsmode&2)?0:MWM_DECOR_MENU);

   XChangeProperty( dpy,w,MotifHints,MotifHints,32,
                    PropModeReplace,(unsigned char *)&MotifWmHints,(fsmode&4)?4:5 );
  }
}

void x11_sizehint(Display *mDisplay, Window window, int x, int y, int width, int height, int max )
{
XSizeHints hint;

 hint.flags=PPosition | PSize | PWinGravity;

 hint.x=x; hint.y=y; hint.width=width; hint.height=height;
 if ( max )
  {
   hint.max_width=width; hint.max_height=height;
   hint.flags|=PMaxSize;
  } else { hint.max_width=0; hint.max_height=0; }
 hint.win_gravity=StaticGravity;
 XSetWMNormalHints( mDisplay,window,&hint );
}

static int x11_get_property(Display *dpy, Window w,
                            Atom type, Atom **args, unsigned long *nitems)
{
  int             format;
  unsigned long   bytesafter;

  return (Success == XGetWindowProperty( dpy,w,type,0,16384,
                                         False,AnyPropertyType,&type,&format,nitems,&bytesafter,
                                         (unsigned char **) args ) && *nitems > 0 );
}

#define wm_LAYER 1
#define wm_FULLSCREEN 2
#define wm_STAYS_ON_TOP 4
#define wm_ABOVE 8
#define wm_BELOW 16
#define wm_NETWM (wm_FULLSCREEN | wm_STAYS_ON_TOP | wm_ABOVE | wm_BELOW)

static int net_wm_support_state_test(Atom atom)
{
#define NET_WM_STATE_TEST(x) { if (atom == XA_NET_WM_STATE_##x) { return wm_##x; } }
 
 NET_WM_STATE_TEST(FULLSCREEN);
 NET_WM_STATE_TEST(ABOVE);
 NET_WM_STATE_TEST(STAYS_ON_TOP);
 NET_WM_STATE_TEST(BELOW);
 return 0;
}

int wm_detect(Display *dpy, Window w)
{
 int             i;
 int             wm = 0;
 int             metacity_hack = 0;
 unsigned long   nitems;
 Atom          * args = NULL;
 
// -- supports layers
  if (x11_get_property(dpy, w,XA_WIN_PROTOCOLS, &args, &nitems))
  {
   for (i = 0; i < nitems; i++)
   {
     if ( args[i] == XA_WIN_LAYER) {
       wm |= wm_LAYER;
       metacity_hack |= 1;
     } else
       // metacity is the only manager mplayer's authors know which reports support only for _WIN_LAYER
       // hint in _WIN_PROTOCOLS (what's more support for it, they say, is broken)
       metacity_hack |= 2;
   }
   XFree( args );
   if (wm && (metacity_hack == 1))
   {
     // metacity reports that it supports layers, but it is not really truth :-)
     wm ^= wm_LAYER;
   }
  }

// --- netwm 
  if (x11_get_property(dpy,w,XA_NET_SUPPORTED, &args, &nitems))
  {
   for (i = 0; i < nitems; i++)
     wm |= net_wm_support_state_test (args[i]);
   XFree( args );
  }

 return wm;
}    

void x11_ontop( Display * dpy,Window w,int fs_type, int fs)
{
   XClientMessageEvent  xev;
   char *state;

   memset( &xev,0,sizeof( xev ) );
   xev.type= 33 ;
   xev.message_type=XA_NET_WM_STATE;
   xev.display=dpy;
   xev.window=w;
   xev.format=32;
   xev.data.l[0]=fs;

   if ( fs_type & wm_STAYS_ON_TOP )
     xev.data.l[1]=XA_NET_WM_STATE_STAYS_ON_TOP;
   else
   if ( fs_type & wm_ABOVE )
     xev.data.l[1]=XA_NET_WM_STATE_ABOVE;
   else
   if ( fs_type & wm_FULLSCREEN )
     xev.data.l[1]=XA_NET_WM_STATE_FULLSCREEN;
   else
   if ( fs_type & wm_BELOW )
     // This is not fallback. We can safely assume that situation where
     // only NETWM_STATE_BELOW is supported and others not, doesn't exist.
     xev.data.l[1]=XA_NET_WM_STATE_BELOW;

   xev.data.l[1]=XA_NET_WM_STATE_STAYS_ON_TOP;
   
   XSendEvent(dpy,RootWindow(dpy,DefaultScreen(dpy)),0 ,(1L<<20) ,(XEvent*)&xev );
   state = XGetAtomName (dpy, xev.data.l[1]);
   XFree (state);
}

