Real Function fd2(dmass, aj, al, width, widb0, em1, em2, srt)
  Save
  amp = em1
  amd = em2
  ak02 = 0.25*(dmass**2-amd**2-amp**2)**2 - (amp*amd)**2
  If (ak02>0.) Then
    q0 = sqrt(ak02/dmass)
  Else
    q0 = 0.0
    fd2 = 0
    Return
  End If
  ak2 = 0.25*(srt**2-amd**2-amp**2)**2 - (amp*amd)**2
  If (ak2>0.) Then
    q = sqrt(ak2/dmass)
  Else
    q = 0.00
    fd2 = 0
    Return
  End If
  b = widb0*1.2*dmass/srt*(q/q0)**(2.*al+1)/(1.+0.2*(q/q0)**(2*al))
  fd2 = (2.*aj+1)*width**2*b/((srt-dmass)**2+0.25*width**2)/(4.*q**2)
  Return
End Function fd2
