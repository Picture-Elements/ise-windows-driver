/*
 * Copyright (c) 1997 Picture Elements, Inc.
 *    Stephen Williams (steve@picturel.com)
 *
 *    This source code is free software; you can redistribute it
 *    and/or modify it in source code form under the terms of the GNU
 *    General Public License as published by the Free Software
 *    Foundation; either version 2 of the License, or (at your option)
 *    any later version. In order to redistribute the software in
 *    binary form, you will need a Picture Elements Binary Software
 *    License.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
 *  ---
 *    You should also have recieved a copy of the Picture Elements
 *    Binary Software License offer along with the source. This offer
 *    allows you to obtain the right to redistribute the software in
 *    binary (compiled) form. If you have not received it, contact
 *    Picture Elements, Inc., 777 Panoramic Way, Berkeley, CA 94704.
 */
#if !defined(WINNT)
#ident "$Id$"
#endif

/*
 * Linux includes proper BIOS32 functions. This file is for the
 * benefit of NT, which has that HAL cruft.
 */

# include  "os.h"


int pcibios_read_config_dword(unsigned char bus, unsigned char dev_fn,
			      unsigned char off, unsigned*val)
{
      unsigned long rc;

      if (off & 0x03) return PCIBIOS_BAD_REGISTER_NUMBER;

      rc = HalGetBusDataByOffset(PCIConfiguration, bus, dev_fn, val,
				 off, sizeof*val);

      switch (rc) {
	  case 0:
	  case 2:
	    return PCIBIOS_BAD_REGISTER_NUMBER;
	  default:
	    return PCIBIOS_SUCCESSFUL;
      }
}

int pcibios_read_config_word(unsigned char bus, unsigned char dev_fn,
			     unsigned char off, unsigned short*val)
{
      unsigned long rc;
      unsigned short tmp[2];

      if (off & 0x01) return PCIBIOS_BAD_REGISTER_NUMBER;

      rc = HalGetBusDataByOffset(PCIConfiguration, bus, dev_fn, tmp,
				 off&~0x3, sizeof tmp);

      switch (rc) {
	  case 0:
	  case 2:
	    return PCIBIOS_BAD_REGISTER_NUMBER;
	  default:
	    *val = tmp[(off/2)%2];
	    return PCIBIOS_SUCCESSFUL;
      }
}

int pcibios_read_config_byte(unsigned char bus, unsigned char dev_fn,
			      unsigned char off, unsigned char*val)
{
      unsigned long rc;
      unsigned char tmp[4];

      rc = HalGetBusDataByOffset(PCIConfiguration, bus, dev_fn, tmp,
				 off&~0x3, sizeof tmp);

      switch (rc) {
	  case 0:
	  case 2:
	    return PCIBIOS_BAD_REGISTER_NUMBER;
	  default:
	    *val = tmp[off%4];
	    return PCIBIOS_SUCCESSFUL;
      }
}

int pcibios_write_config_dword(unsigned char bus, unsigned char dev_fn,
			       unsigned char off, unsigned val)
{
      unsigned long rc;

      if (off & 0x03) return PCIBIOS_BAD_REGISTER_NUMBER;

      rc = HalSetBusDataByOffset(PCIConfiguration, bus, dev_fn, &val,
				 off, sizeof val);

      if (rc != sizeof val)
	    return PCIBIOS_BAD_REGISTER_NUMBER;
      else
	    return PCIBIOS_SUCCESSFUL;
}

int pcibios_write_config_byte(unsigned char bus, unsigned char dev_fn,
			      unsigned char off, unsigned char val)
{
      unsigned long rc;

      rc = HalSetBusDataByOffset(PCIConfiguration, bus, dev_fn, &val,
				 off, sizeof val);

      if (rc != sizeof val)
	    return PCIBIOS_BAD_REGISTER_NUMBER;
      else
	    return PCIBIOS_SUCCESSFUL;
}

/*
 * $Log$
 * Revision 1.1  2001/03/05 20:11:40  steve
 *  Add NT4 driver to ISE source tree.
 *
 * Revision 1.1  1998/05/28 22:53:01  steve
 *  NT port.
 *
 */

