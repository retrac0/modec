#!/usr/bin/env python3
"""Textbook error rates for the bench's modulations, on the bench's SNR.

The bench's SNR is the one Modec.Channel and --line-snr define: white
noise over the whole 0-4 kHz band, sigma the signal's r.m.s. over
10^(snr/20).  A receiver's matched filter keeps only the noise inside
its own band, so the energy per bit over noise density is

    Eb/N0 [dB] = SNR [dB] + 10 log10(4000 / bit rate)

which is what turns 300 bit/s FSK into something that reads at 0 dB.
The curves are the standard AWGN results; the trellis rates are given
as the uncoded constellation of the same size plus the asymptotic
coding gain of the 8-state Ungerboeck code, which is the usual rule of
thumb and no better than one.
"""
import math

def Q(x):
    return 0.5 * math.erfc(x / math.sqrt(2))

def ebn0(snr_db, rate):
    return snr_db + 10 * math.log10(4000.0 / rate)

def ber(mode, snr_db):
    """Bit error rate at the bench's SNR, per mode tag as ber.py names them."""
    if mode in ('bell103', 'v21'):
        # binary FSK, non-coherently detected, orthogonal tones: Pb = 1/2 exp(-Eb/2N0)
        g = 10 ** (ebn0(snr_db, 300) / 10)
        return 0.5 * math.exp(-g / 2)
    if mode in ('bell212a', 'v22'):
        # 4-DPSK at 600 baud: coherent QPSK with differential decoding, ~2x the errors
        g = 10 ** (ebn0(snr_db, 1200) / 10)
        return 2 * Q(math.sqrt(2 * g))
    if mode == 'v32-4800':
        # four points, 2400 baud, differentially coded quadrants
        g = 10 ** (ebn0(snr_db, 4800) / 10)
        return 2 * Q(math.sqrt(2 * g))
    if mode == 'v22bis':
        # 16-QAM, 600 baud, Gray-ish with differential quadrant: ~ (3/4) Q(sqrt(4/5 Eb/N0)) x 1.5 for the differential pair
        g = 10 ** (ebn0(snr_db, 2400) / 10)
        return 1.5 * 0.75 * Q(math.sqrt(0.8 * g))
    if mode == 'v32-9600':
        # 16 points non-redundant, 2400 baud
        g = 10 ** (ebn0(snr_db, 9600) / 10)
        return 1.5 * 0.75 * Q(math.sqrt(0.8 * g))
    # trellis rates: uncoded constellation of the payload's size, plus the code's gain
    gains = {'v32b-9600t': (16, 9600, 3.0), 'v32b-7200': (8, 7200, 3.0),
             'v32b-12000': (32, 12000, 3.0), 'v32b-14400': (64, 14400, 3.0)}
    if mode in gains:
        M, rate, gain = gains[mode]
        g = 10 ** ((ebn0(snr_db, rate) + gain) / 10)
        k = math.log2(M)
        # square/cross M-QAM approximation: Ps ~ 4(1-1/sqrt(M)) Q(sqrt(3k/(M-1) Eb/N0)), Pb ~ Ps/k
        ps = 4 * (1 - 1 / math.sqrt(M)) * Q(math.sqrt(3 * k / (M - 1) * g))
        return min(0.5, ps / k)
    raise KeyError(mode)

def snr_for(mode, target, lo=-20.0, hi=60.0):
    """The SNR at which the textbook curve crosses a bit error rate."""
    for _ in range(60):
        mid = (lo + hi) / 2
        if ber(mode, mid) > target: lo = mid
        else: hi = mid
    return (lo + hi) / 2

if __name__ == '__main__':
    modes = ['bell103','v21','bell212a','v22','v22bis','v32-4800','v32-9600','v32b-9600t','v32b-7200','v32b-12000','v32b-14400']
    print('mode          Eb/N0 offset   SNR for BER 1e-2   1e-3   1e-4   1e-5')
    rates = {'bell103':300,'v21':300,'bell212a':1200,'v22':1200,'v22bis':2400,'v32-4800':4800,'v32-9600':9600,
             'v32b-9600t':9600,'v32b-7200':7200,'v32b-12000':12000,'v32b-14400':14400}
    for m in modes:
        print(f'{m:12}  {10*math.log10(4000/rates[m]):+5.1f} dB      ' +
              '  '.join(f'{snr_for(m, t):5.1f}' for t in (1e-2, 1e-3, 1e-4, 1e-5)))
