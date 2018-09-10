Real Function w1535(dmass)
  Save
  avmass = 0.938868
  pimass = 0.137265
  aux = 0.25*(dmass**2-avmass**2-pimass**2)**2 - (avmass*pimass)**2
  If (aux>0.) Then
     qavail = sqrt(aux/dmass**2)
  Else
     qavail = 1.E-06
  End If
  w1535 = 0.15*qavail/0.467
  Return
End Function w1535
