Real Function dirct1(srt)
  Real xarray(122), earray(122)
  Save
  Data earray/1.568300, 1.578300, 1.588300, 1.598300, 1.608300, 1.618300, 1.628300, 1.638300, 1.648300, 1.658300, 1.668300, 1.678300, 1.688300, 1.698300, 1.708300, 1.718300, 1.728300, 1.738300, 1.748300, 1.758300, 1.768300, 1.778300, 1.788300, 1.798300, 1.808300, 1.818300, 1.828300, 1.838300, 1.848300, 1.858300, 1.868300, 1.878300, 1.888300, 1.898300, 1.908300, 1.918300, 1.928300, 1.938300, 1.948300, 1.958300, 1.968300, 1.978300, 1.988300, 1.998300, 2.008300, 2.018300, 2.028300, 2.038300, 2.048300, 2.058300, 2.068300, 2.078300, 2.088300, 2.098300, 2.108300, 2.118300, 2.128300, 2.138300, 2.148300, 2.158300, 2.168300, 2.178300, 2.188300, 2.198300, 2.208300, 2.218300, 2.228300, 2.238300, 2.248300, 2.258300, 2.268300, 2.278300, 2.288300, 2.298300, 2.308300, 2.318300, 2.328300, 2.338300, 2.348300, 2.358300, 2.368300, 2.378300, 2.388300, 2.398300, 2.408300, 2.418300, 2.428300, 2.438300, 2.448300, 2.458300, 2.468300, 2.478300, 2.488300, 2.498300, 2.508300, 2.518300, 2.528300, 2.538300, 2.548300, 2.558300, 2.568300, &
    2.578300, 2.588300, 2.598300, 2.608300, 2.618300, 2.628300, 2.638300, 2.648300, 2.658300, 2.668300, 2.678300, 2.688300, 2.698300, 2.708300, 2.718300, 2.728300, 2.738300, 2.748300, 2.758300, 2.768300, 2.778300/
  Data xarray/1.7764091E-02, 0.5643668, 0.8150568, 1.045565, 2.133695, 3.327922, 4.206488, 3.471242, 4.486876, 5.542213, 6.800052, 7.192446, 6.829848, 6.580306, 6.868410, 8.527946, 10.15720, 9.716511, 9.298335, 8.901310, 10.31213, 10.52185, 11.17630, 11.61639, 12.05577, 12.71596, 13.46036, 14.22060, 14.65449, 14.94775, 14.93310, 15.32907, 16.56481, 16.29422, 15.18548, 14.12658, 13.72544, 13.24488, 13.31003, 14.42680, 12.84423, 12.49025, 12.14858, 11.81870, 11.18993, 11.35816, 11.09447, 10.83873, 10.61592, 10.53754, 9.425521, 8.195912, 9.661075, 9.696192, 9.200142, 8.953734, 8.715461, 8.484999, 8.320765, 8.255512, 8.190969, 8.127125, 8.079508, 8.073004, 8.010611, 7.948909, 7.887895, 7.761005, 7.626290, 7.494696, 7.366132, 7.530178, 8.392097, 9.046881, 8.962544, 8.879403, 8.797427, 8.716601, 8.636904, 8.558312, 8.404368, 8.328978, 8.254617, 8.181265, 8.108907, 8.037527, 7.967100, 7.897617, 7.829057, 7.761405, 7.694647, 7.628764, 7.563742, 7.499570, 7.387562, 7.273281, 7.161334, 6.973375, 6.529592, 6.280323, &
    6.293136, 6.305725, 6.318097, 6.330258, 6.342214, 6.353968, 6.365528, 6.376895, 6.388079, 6.399081, 6.409906, 6.420560, 6.431045, 6.441367, 6.451529, 6.461533, 6.471386, 6.481091, 6.490650, 6.476413, 6.297259, 6.097826/
  If (srt<earray(1)) Then
    dirct1 = 0.00001
    Return
  End If
  If (srt>earray(122)) Then
    dirct1 = xarray(122)
    dirct1 = dirct1/10.
    Return
  End If
  Do ie = 1, 122
    If (earray(ie)==srt) Then
      dirct1 = xarray(ie)
      dirct1 = dirct1/10.
      Return
    End If
    If (earray(ie)>srt) Then
      ymin = alog(xarray(ie-1))
      ymax = alog(xarray(ie))
      xmin = alog(earray(ie-1))
      xmax = alog(earray(ie))
      dirct1 = exp(ymin+(alog(srt)-xmin)*(ymax-ymin)/(xmax-xmin))
      dirct1 = dirct1/10.
      Goto 50
    End If
  End Do
  50 Continue
  Return
End Function dirct1
