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


/* Window manager manipulation functions to handle "fullscreen" mode */
/* The code from here to gr_reposition is extracted and reorganized
   from the WM detection logic and fullscreen routines of mplayer    */

/* TODO : add motif detection to configure */

#ifdef __grwm__

#else

#define __grwm__

void x11_decoration(Display *dpy, Window w, int decorate, int fsmode);
void x11_sizehint(Display *mDisplay, Window window, 
                  int x, int y, int width, int height, int max);
void x11_ontop(Display * dpy, Window w, int fs_type, int fs);
int wm_detect(Display *dpy, Window w);
void init_atoms(Display * mDisplay);

#endif
