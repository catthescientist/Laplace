Public Class Form1
    Dim MX1 As New Matrix
    Dim curve1 As New curve
    Dim pic1 As Graphics
    Dim pic2 As Graphics
    Dim curves() As curve
    Dim taumas() As Double

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Dim x, y As Short
        x = CShort(TextBox1.Text) - 1
        y = CShort(TextBox2.Text) - 1
        Dim a(x, y) As Double
        RichTextBox1.Visible = True
        PictureBox1.Visible = False
        RichTextBox1.Clear()

        Array.Copy(generatearray(x, y), a, a.Length)
        printmx(a)
        MX1.loadarray(a)
        printmx(MX1.dotproduct(MX1.getarray, MX1.Invert))
        MX1.loadarray(a)
        printmx(MX1.Upper)
        MsgBox(MX1.getarray(3, 1))

        'MsgBox(MX1.n_of_cols.ToString + " cols")
        'MsgBox(MX1.n_of_rows.ToString + " rows")

        'printmx(MX1.transpose(a))
        'MX1.loadarray(a)
        'printmx(MX1.Upper)
        'printmx(MX1.Invert)
        'printmx(MX1.dotproduct(MX1.getarray, MX1.Invert))

        'Array.Copy(generatearray(y, x), b, b.Length)
        'printmx(b)

        'Array.Copy(MX1.Invert(), b, ((x + 1) * (y + 1)))
        'printmx(MX1.Invert())

        'Array.Copy(MX1.dotproduct(a, a), c, c.Length)
        'printmx(MX1.dotproduct(a, b))

    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        'RichTextBox1.Visible = False
        'PictureBox1.Visible = True
        pic1.Clear(Color.White)
        pic2.Clear(Color.White)
        drawaxis()
        Dim tstart As Double = 0 'CDbl(TextBox3.Text)
        Dim tfin As Double = CDbl(TextBox4.Text)
        Dim nofp As Short = CShort(TextBox5.Text)
        Dim b(nofp - 1, 1) As Double

        Dim i As Short
        For i = 0 To nofp - 1 Step 1
            'b(i, 0) = tstart + (tfin - tstart) / (nofp) * (i + 1)
            'b(i, 0) = tfin / nofp * (nofp - i)
            b(i, 0) = tfin / nofp * (i + 1)
            b(i, 1) = CDbl(TextBox10.Text)
            b(i, 1) = b(i, 1) + CDbl(TextBox6.Text) * Math.Exp(-b(i, 0) / CDbl(TextBox7.Text))
            b(i, 1) = b(i, 1) + CDbl(TextBox8.Text) * Math.Exp(-b(i, 0) / CDbl(TextBox9.Text))
            'dot(b(i, 0), b(i, 1))
        Next i

        'b(0, 0) = 0.03 : b(0, 1) = 0.360599999999998
        'b(1, 0) = 0.06 : b(1, 1) = 0.343799999999998
        'b(2, 0) = 0.09 : b(2, 1) = 0.348599999999998
        'b(3, 0) = 0.12 : b(3, 1) = 0.342199999999998
        'b(4, 0) = 0.15 : b(4, 1) = 0.308609999999998
        'b(5, 0) = 0.18 : b(5, 1) = 0.281049999999997
        'b(6, 0) = 0.21 : b(6, 1) = 0.264089999999999
        'b(7, 0) = 0.24 : b(7, 1) = 0.255649999999999
        'b(8, 0) = 0.27 : b(8, 1) = 0.248619999999999
        'b(9, 0) = 0.3 : b(9, 1) = 0.238149999999997
        'b(10, 0) = 0.33 : b(10, 1) = 0.227459999999997
        'b(11, 0) = 0.36 : b(11, 1) = 0.211039999999997
        'b(12, 0) = 0.39 : b(12, 1) = 0.195529999999998
        'b(13, 0) = 0.42 : b(13, 1) = 0.185389999999998
        'b(14, 0) = 0.45 : b(14, 1) = 0.183349999999997
        'b(15, 0) = 0.48 : b(15, 1) = 0.173559999999998
        'b(16, 0) = 0.51 : b(16, 1) = 0.169059999999998
        'b(17, 0) = 0.54 : b(17, 1) = 0.164459999999998
        'b(18, 0) = 0.57 : b(18, 1) = 0.154609999999998
        'b(19, 0) = 0.6 : b(19, 1) = 0.146049999999999
        'b(20, 0) = 0.63 : b(20, 1) = 0.14066
        'b(21, 0) = 0.66 : b(21, 1) = 0.13533
        'b(22, 0) = 0.69 : b(22, 1) = 0.128699999999998
        'b(23, 0) = 0.72 : b(23, 1) = 0.124279999999999
        'b(24, 0) = 0.75 : b(24, 1) = 0.116719999999997
        'b(25, 0) = 0.78 : b(25, 1) = 0.116049999999998
        'b(26, 0) = 0.81 : b(26, 1) = 0.118499999999997
        'b(27, 0) = 0.84 : b(27, 1) = 0.116159999999997
        'b(28, 0) = 0.87 : b(28, 1) = 0.110509999999998
        'b(29, 0) = 0.9 : b(29, 1) = 0.102239999999998
        'b(30, 0) = 0.93 : b(30, 1) = 0.0922799999999988
        'b(31, 0) = 0.96 : b(31, 1) = 0.0839399999999984
        'b(32, 0) = 0.99 : b(32, 1) = 0.0842499999999973
        'b(33, 0) = 1.02 : b(33, 1) = 0.082849999999997
        'b(34, 0) = 1.05 : b(34, 1) = 0.0823900000000002
        'b(35, 0) = 1.08 : b(35, 1) = 0.0786199999999973
        'b(36, 0) = 1.11 : b(36, 1) = 0.0735799999999998
        'b(37, 0) = 1.14 : b(37, 1) = 0.0670099999999998
        'b(38, 0) = 1.17 : b(38, 1) = 0.066989999999997
        'b(39, 0) = 1.2 : b(39, 1) = 0.0693400000000004
        'b(40, 0) = 1.23 : b(40, 1) = 0.0695599999999992
        'b(41, 0) = 1.26 : b(41, 1) = 0.0721499999999971
        'b(42, 0) = 1.29 : b(42, 1) = 0.0680399999999999
        'b(43, 0) = 1.32 : b(43, 1) = 0.0690099999999987
        'b(44, 0) = 1.35 : b(44, 1) = 0.0597599999999972
        'b(45, 0) = 1.38 : b(45, 1) = 0.0471699999999977
        'b(46, 0) = 1.41 : b(46, 1) = 0.0410000000000004
        'b(47, 0) = 1.44 : b(47, 1) = 0.040619999999997
        'b(48, 0) = 1.47 : b(48, 1) = 0.0372500000000002
        'b(49, 0) = 1.5 : b(49, 1) = 0.0410000000000004
        'b(50, 0) = 1.53 : b(50, 1) = 0.0397099999999995
        'b(51, 0) = 1.56 : b(51, 1) = 0.0358799999999988
        'b(52, 0) = 1.59 : b(52, 1) = 0.0394100000000002
        'b(53, 0) = 1.62 : b(53, 1) = 0.0415299999999981
        'b(54, 0) = 1.65 : b(54, 1) = 0.0389099999999978
        'b(55, 0) = 1.68 : b(55, 1) = 0.0371699999999997
        'b(56, 0) = 1.71 : b(56, 1) = 0.0364799999999974
        'b(57, 0) = 1.74 : b(57, 1) = 0.0324899999999992
        'b(58, 0) = 1.77 : b(58, 1) = 0.031489999999998
        'b(59, 0) = 1.8 : b(59, 1) = 0.032239999999998
        'b(60, 0) = 1.83 : b(60, 1) = 0.0268999999999977
        'b(61, 0) = 1.86 : b(61, 1) = 0.0252400000000002
        'b(62, 0) = 1.89 : b(62, 1) = 0.0253399999999999
        'b(63, 0) = 1.92 : b(63, 1) = 0.0234099999999984
        'b(64, 0) = 1.95 : b(64, 1) = 0.0259
        'b(65, 0) = 1.98 : b(65, 1) = 0.0252400000000002
        'b(66, 0) = 2.01 : b(66, 1) = 0.0183900000000001
        'b(67, 0) = 2.04 : b(67, 1) = 0.0146199999999972
        'b(68, 0) = 2.07 : b(68, 1) = 0.0148399999999995
        'b(69, 0) = 2.1 : b(69, 1) = 0.0145099999999978
        'b(70, 0) = 2.13 : b(70, 1) = 0.021099999999997
        'b(71, 0) = 2.16 : b(71, 1) = 0.0243499999999983
        'b(72, 0) = 2.19 : b(72, 1) = 0.020629999999997
        'b(73, 0) = 2.22 : b(73, 1) = 0.0158699999999996
        'b(74, 0) = 2.25 : b(74, 1) = 0.00924999999999798
        'b(75, 0) = 2.28 : b(75, 1) = 0.00537999999999883
        'b(76, 0) = 2.31 : b(76, 1) = 0.00859999999999772
        'b(77, 0) = 2.34 : b(77, 1) = 0.0125399999999978
        'b(78, 0) = 2.37 : b(78, 1) = 0.0154499999999977
        'b(79, 0) = 2.4 : b(79, 1) = 0.0123999999999995
        'b(80, 0) = 2.43 : b(80, 1) = 0.0148399999999995
        'b(81, 0) = 2.46 : b(81, 1) = 0.0212799999999973
        'b(82, 0) = 2.49 : b(82, 1) = 0.0220500000000001
        'b(83, 0) = 2.52 : b(83, 1) = 0.0198699999999974
        'b(84, 0) = 2.55 : b(84, 1) = 0.012789999999999
        'b(85, 0) = 2.58 : b(85, 1) = 0.00528999999999868
        'b(86, 0) = 2.61 : b(86, 1) = 0.00989999999999824
        'b(87, 0) = 2.64 : b(87, 1) = 0.0147799999999982
        'b(88, 0) = 2.67 : b(88, 1) = 0.0197699999999976
        'b(89, 0) = 2.7 : b(89, 1) = 0.0182099999999998
        'b(90, 0) = 2.73 : b(90, 1) = 0.00789999999999935
        'b(91, 0) = 2.76 : b(91, 1) = 0.00915999999999784
        'b(92, 0) = 2.79 : b(92, 1) = 0.0118699999999983
        'b(93, 0) = 2.82 : b(93, 1) = 0.0164099999999969
        'b(94, 0) = 2.85 : b(94, 1) = 0.0182000000000002
        'b(95, 0) = 2.88 : b(95, 1) = 0.0111999999999988
        'b(96, 0) = 2.91 : b(96, 1) = 0.00590000000000046
        'b(97, 0) = 2.94 : b(97, 1) = 0.00159999999999982
        'b(98, 0) = 2.97 : b(98, 1) = 0.00323999999999813
        'b(99, 0) = 3 : b(99, 1) = 0.00820999999999827
        'b(100, 0) = 3.03 : b(100, 1) = 0.01173
        'b(101, 0) = 3.06 : b(101, 1) = 0.0146699999999989
        'b(102, 0) = 3.09 : b(102, 1) = 0.021729999999998
        'b(103, 0) = 3.12 : b(103, 1) = 0.0153599999999976
        'b(104, 0) = 3.15 : b(104, 1) = 0.0145499999999998
        'b(105, 0) = 3.18 : b(105, 1) = 0.00707999999999842
        'b(106, 0) = 3.21 : b(106, 1) = -0.00112999999999985
        'b(107, 0) = 3.24 : b(107, 1) = -0.00679000000000229
        'b(108, 0) = 3.27 : b(108, 1) = -0.00566000000000244
        'b(109, 0) = 3.3 : b(109, 1) = -0.00866000000000255
        'b(110, 0) = 3.33 : b(110, 1) = 0.0000799999999969714
        'b(111, 0) = 3.36 : b(111, 1) = -0.00270000000000081
        'b(112, 0) = 3.39 : b(112, 1) = -0.00744000000000256
        'b(113, 0) = 3.42 : b(113, 1) = -0.00778000000000034
        'b(114, 0) = 3.45 : b(114, 1) = -0.00600999999999985
        'b(115, 0) = 3.48 : b(115, 1) = 0.000659999999999883
        'b(116, 0) = 3.51 : b(116, 1) = 0.00935999999999737
        'b(117, 0) = 3.54 : b(117, 1) = 0.0131199999999971
        'b(118, 0) = 3.57 : b(118, 1) = 0.0105199999999996
        'b(119, 0) = 3.6 : b(119, 1) = 0.00615999999999772
        'b(120, 0) = 3.63 : b(120, 1) = -0.000610000000001776
        'b(121, 0) = 3.66 : b(121, 1) = 0.00509999999999877
        'b(122, 0) = 3.69 : b(122, 1) = 0.0046999999999997
        'b(123, 0) = 3.72 : b(123, 1) = 0.0132999999999974
        'b(124, 0) = 3.75 : b(124, 1) = 0.00701999999999714
        'b(125, 0) = 3.78 : b(125, 1) = 0.00309000000000026
        'b(126, 0) = 3.81 : b(126, 1) = -0.00634000000000157
        'b(127, 0) = 3.84 : b(127, 1) = -0.00744000000000256
        'b(128, 0) = 3.87 : b(128, 1) = -0.00488
        'b(129, 0) = 3.9 : b(129, 1) = 0.00189999999999912
        'b(130, 0) = 3.93 : b(130, 1) = 0.00268999999999764
        'b(131, 0) = 3.96 : b(131, 1) = 0.0106399999999987
        'b(132, 0) = 3.99 : b(132, 1) = 0.00609999999999999
        'b(133, 0) = 4.02 : b(133, 1) = 0.00979999999999848
        'b(134, 0) = 4.05 : b(134, 1) = 0.0105499999999985
        'b(135, 0) = 4.08 : b(135, 1) = 0.00514999999999688
        'b(136, 0) = 4.11 : b(136, 1) = 0.00179999999999936
        'b(137, 0) = 4.14 : b(137, 1) = -0.00394000000000005
        'b(138, 0) = 4.17 : b(138, 1) = -0.0140200000000021
        'b(139, 0) = 4.2 : b(139, 1) = -0.0100800000000021
        'b(140, 0) = 4.23 : b(140, 1) = -0.00894000000000261
        'b(141, 0) = 4.26 : b(141, 1) = -0.000659999999999883
        'b(142, 0) = 4.29 : b(142, 1) = 0.00840999999999781
        'b(143, 0) = 4.32 : b(143, 1) = 0.00889999999999702
        'b(144, 0) = 4.35 : b(144, 1) = 0.0064700000000002
        'b(145, 0) = 4.38 : b(145, 1) = 0.00698999999999828
        'b(146, 0) = 4.41 : b(146, 1) = 0.00560999999999723
        'b(147, 0) = 4.44 : b(147, 1) = 0.00380999999999787
        'b(148, 0) = 4.47 : b(148, 1) = 0.00557999999999836
        'b(149, 0) = 4.5 : b(149, 1) = 0.00347999999999971
        'b(150, 0) = 4.53 : b(150, 1) = 0.00377999999999901
        'b(151, 0) = 4.56 : b(151, 1) = 0.00295999999999808
        'b(152, 0) = 4.59 : b(152, 1) = 0.00917999999999708
        'b(153, 0) = 4.62 : b(153, 1) = 0.00283999999999907
        'b(154, 0) = 4.65 : b(154, 1) = 0.00322999999999851
        'b(155, 0) = 4.68 : b(155, 1) = 0.00170999999999921
        'b(156, 0) = 4.71 : b(156, 1) = -0.00272000000000006
        'b(157, 0) = 4.74 : b(157, 1) = -0.00740000000000052
        'b(158, 0) = 4.77 : b(158, 1) = -0.00298000000000087
        'b(159, 0) = 4.8 : b(159, 1) = -0.00330000000000297
        'b(160, 0) = 4.83 : b(160, 1) = 0.0000799999999969714
        'b(161, 0) = 4.86 : b(161, 1) = 0.0028100000000002
        'b(162, 0) = 4.89 : b(162, 1) = -0.00499000000000294
        'b(163, 0) = 4.92 : b(163, 1) = -0.00578000000000145
        'b(164, 0) = 4.95 : b(164, 1) = -0.00259000000000142
        'b(165, 0) = 4.98 : b(165, 1) = 0.0024899999999981
        'b(166, 0) = 5.01 : b(166, 1) = 0.0112699999999997
        'b(167, 0) = 5.04 : b(167, 1) = 0.00919999999999988
        'b(168, 0) = 5.07 : b(168, 1) = -0.00215000000000032
        'b(169, 0) = 5.1 : b(169, 1) = -0.00447000000000131
        'b(170, 0) = 5.13 : b(170, 1) = 0.00113999999999947
        'b(171, 0) = 5.16 : b(171, 1) = -0.0015900000000002
        'b(172, 0) = 5.19 : b(172, 1) = 0.00526999999999944
        'b(173, 0) = 5.22 : b(173, 1) = 0.00150999999999968
        'b(174, 0) = 5.25 : b(174, 1) = -0.00291999999999959
        'b(175, 0) = 5.28 : b(175, 1) = -0.00715000000000288
        'b(176, 0) = 5.31 : b(176, 1) = -0.00322000000000244
        'b(177, 0) = 5.34 : b(177, 1) = -0.00854999999999961
        'b(178, 0) = 5.37 : b(178, 1) = -0.00740000000000052
        'b(179, 0) = 5.4 : b(179, 1) = -0.00543000000000049
        'b(180, 0) = 5.43 : b(180, 1) = -0.00459000000000032
        'b(181, 0) = 5.46 : b(181, 1) = 0.00189999999999912
        'b(182, 0) = 5.49 : b(182, 1) = 0.00226999999999933
        'b(183, 0) = 5.52 : b(183, 1) = 0.000689999999998747
        'b(184, 0) = 5.55 : b(184, 1) = -0.00105000000000288
        'b(185, 0) = 5.58 : b(185, 1) = 0.0000399999999984857
        'b(186, 0) = 5.61 : b(186, 1) = -0.000390000000002999
        'b(187, 0) = 5.64 : b(187, 1) = 0.00236999999999909
        'b(188, 0) = 5.67 : b(188, 1) = -0.000550000000000495
        'b(189, 0) = 5.7 : b(189, 1) = 0.000809999999997757
        'b(190, 0) = 5.73 : b(190, 1) = 0.000979999999998427
        'b(191, 0) = 5.76 : b(191, 1) = 0.00497999999999976
        'b(192, 0) = 5.79 : b(192, 1) = 0.00884999999999891
        'b(193, 0) = 5.82 : b(193, 1) = 0.00561999999999685
        'b(194, 0) = 5.85 : b(194, 1) = -0.000580000000002912
        'b(195, 0) = 5.88 : b(195, 1) = 0.000180000000000291
        'b(196, 0) = 5.91 : b(196, 1) = -0.00346000000000046
        'b(197, 0) = 5.94 : b(197, 1) = -0.00187000000000026
        'b(198, 0) = 5.97 : b(198, 1) = -0.00974000000000075
        'b(199, 0) = 6 : b(199, 1) = 0.00229999999999819





        curve1.loadcurve(b)
        drawcurve(curve1.getcurve)
        taudot(CDbl(TextBox7.Text), CDbl(TextBox6.Text))
        taudot(CDbl(TextBox9.Text), CDbl(TextBox8.Text))
    End Sub

    Private Sub printmx(ByVal mx(,) As Double)
        Dim textline As String
        Dim i, j As Short
        For i = 0 To (mx.GetLength(0) - 1) Step 1
            textline = ""
            For j = 0 To (mx.GetLength(1) - 1) Step 1
                textline = textline + CStr(Format(mx(i, j), "#.####")) + " | "
            Next j
            textline = textline + vbNewLine
            RichTextBox1.Text = RichTextBox1.Text + textline
        Next i
        RichTextBox1.Text = RichTextBox1.Text + vbNewLine
    End Sub

    Private Sub drawaxis()
        pic1.DrawLine(Pens.Black, 50, 10, 50, 390)
        pic1.DrawLine(Pens.Black, 10, 350, 490, 350)
        pic1.DrawLine(Pens.Black, 45, 50, 55, 50)
        Dim i As Short
        For i = 50 To 410 Step 60
            pic1.DrawLine(Pens.Black, 50, i, 55, i)
        Next i
        For i = 50 To 450 Step 100
            pic1.DrawLine(Pens.Black, i, 345, i, 355)
        Next i
        For i = 30 To 470 Step 20
            pic1.DrawLine(Pens.Black, i, 345, i, 350)
        Next i

        pic2.DrawLine(Pens.Black, 10, 100, 780, 100)
        For i = 100 To 700 Step 200
            pic2.DrawLine(Pens.Black, i, 95, i, 105)
        Next i
    End Sub

    Private Sub drawcurve(ByVal curvemas(,) As Double)
        Dim i As Short
        For i = 0 To (curvemas.GetLength(0) - 1) Step 1
            dot(curvemas(i, 0), curvemas(i, 1))
        Next i
    End Sub

    Private Sub dot(ByVal x, ByVal y)
        Dim xsng As Single = CSng(x)
        Dim ysng As Single = CSng(y)
        Dim unx As Single = 50
        Dim uny As Single = 300
        Dim startx As Single = 50
        Dim starty As Single = 350
        pic1.DrawEllipse(Pens.Black, xsng * unx + startx - 1, starty - ysng * uny - 1, 2, 2)
    End Sub

    Private Sub taudot(ByVal x, ByVal y)
        Dim xsng As Single = CSng(Math.Log10(x))
        Dim ysng As Single = CSng(y)
        Dim unx As Single = 200
        Dim uny As Single = 50
        Dim startx As Single = 700
        Dim starty As Single = 100
        If Math.Abs(y) < 10000 Then
            pic2.DrawEllipse(Pens.Black, xsng * unx + startx - 1, starty - ysng * uny - 1, 2, 2)
        End If

    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        'PictureBox1.Visible = False
        RichTextBox1.Visible = True
        Dim alpha As Double
        ReDim taumas(CShort(TextBox13.Text))
        Dim coefsA(0, CShort(TextBox13.Text) + 1) As Double
        Dim newsignal(curve1.getnofpoints) As Double
        Dim i, j As Short
        For i = 0 To CShort(TextBox13.Text) Step 1
            taumas(i) = Math.Log(CDbl(TextBox12.Text))
            taumas(i) = taumas(i) + (Math.Log(CDbl(TextBox11.Text)) - Math.Log(CDbl(TextBox12.Text))) * i / CShort(TextBox13.Text)
            taumas(i) = Math.Exp(taumas(i))
        Next i

        Dim coefsC(curve1.getnofpoints - 1, taumas.Length) As Double
        Array.Copy(generatecoefs(curve1.gettimes, taumas), coefsC, coefsC.Length)
        Dim signalmas(curve1.getnofpoints - 1, 0) As Double
        For i = 0 To curve1.getnofpoints - 1 Step 1
            signalmas(i, 0) = curve1.getsignals(i) '* (i + 1) '* curve1.gettimes(i) * curve1.gettimes(i)  '*****
        Next i
        'Array.Copy(curve1.getsignals, signalmas, curve1.getnofpoints)

        'Dim tempmas(taumas.Length, taumas.Length) As Double
        'Array.Copy(MX1.dotproduct(MX1.transpose(coefsC), coefsC), tempmas, tempmas.Length)

        printmx(MX1.dotproduct(MX1.transpose(coefsC), coefsC))
        printmx(MX1.Invert(MX1.dotproduct(MX1.transpose(coefsC), coefsC)))
        printmx(MX1.dotproduct(MX1.dotproduct(MX1.transpose(coefsC), coefsC), MX1.Invert(MX1.dotproduct(MX1.transpose(coefsC), coefsC))))
        printmx(MX1.dotproduct(MX1.transpose(coefsC), signalmas))
        printmx(MX1.dotproduct(MX1.Invert(MX1.dotproduct(MX1.transpose(coefsC), coefsC)), MX1.dotproduct(MX1.transpose(coefsC), signalmas)))

        Array.Copy(MX1.dotproduct(MX1.Invert(MX1.dotproduct(MX1.transpose(coefsC), coefsC)), MX1.dotproduct(MX1.transpose(coefsC), signalmas)), coefsA, coefsA.Length)

        For i = 0 To (curve1.getnofpoints - 1) Step 1
            newsignal(i) = 0
            For j = 0 To (taumas.Length - 1) Step 1
                newsignal(i) = newsignal(i) + coefsA(0, j) * Math.Exp(-curve1.gettimes(i) / taumas(j))
            Next j
            newsignal(i) = newsignal(i) + coefsA(0, taumas.Length)
        Next i

        Dim newcurve(curve1.getnofpoints - 1, 1) As Double
        For i = 0 To curve1.getnofpoints - 1 Step 1
            newcurve(i, 0) = curve1.gettimes(i)
            newcurve(i, 1) = newsignal(i) '/ (i + 1) '/ curve1.gettimes(i) / curve1.gettimes(i) '*****
        Next i

        drawcurve(newcurve)
        For i = 0 To taumas.Length - 1 Step 1
            taudot(taumas(i), coefsA(0, i))
        Next i
    End Sub

    Private Function generatecoefs(ByVal timemas() As Double, ByVal taumas() As Double)
        Dim coefs(timemas.Length - 1, taumas.Length) As Double
        Dim i, j As Short

        For i = 0 To (timemas.Length - 1) Step 1
            For j = 0 To (taumas.Length - 1) Step 1
                coefs(i, j) = Math.Exp(-timemas(i) / taumas(j)) '* (i + 1) '* timemas(i) * timemas(i)  '*****
            Next j
            coefs(i, taumas.Length) = 1 '* (i + 1) '* timemas(i) * timemas(i)  '*****
        Next i

        Return coefs
    End Function

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        Me.Close()
    End Sub

    Public Function generatearray(ByVal x_col As Short, ByVal y_row As Short)
        Dim i, j As Short
        Dim MX(x_col, y_row) As Double
        For i = 0 To x_col Step 1
            For j = 0 To y_row Step 1
                MX(i, j) = Rnd()
            Next j
        Next i
        Return MX
    End Function

    Public Sub New()
        ' This call is required by the Windows Form Designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        pic1 = PictureBox1.CreateGraphics()
        pic2 = PictureBox2.CreateGraphics()
    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        'MsgBox(1000000 - 1000000 + 0.0000000000000001)
        RichTextBox1.Clear()
        Dim a As Single
        Dim b As Double
        Dim c As Decimal
        Dim d As Single
        d = 0.0000000000000002
        a = d + 1
        b = d + 1
        c = d + 1
        a = a - 1
        b = b - 1
        c = c - 1
        'MsgBox(a.ToString + " " + b.ToString + " " + c.ToString)
    End Sub
End Class

Public Class Matrix
    Private MX_(,) As Double
    Private UMX_(,) As Double
    Private IMX_(,) As Double
    Private det_ As Double

    Public ReadOnly Property Upper()
        Get
            Return UMX_
        End Get
    End Property

    Public Function Invert(Optional ByVal tempmatrix(,) As Double = Nothing)
        If tempmatrix Is Nothing Then
            Return IMX_
        Else
            Return ext_inv_calc(tempmatrix)
        End If
    End Function

    Public ReadOnly Property det()
        Get
            Return det_
        End Get
    End Property

    Public ReadOnly Property getarray()
        Get
            Return MX_
        End Get
    End Property

    Public ReadOnly Property n_of_cols()
        Get
            Return MX_.GetLength(1)
        End Get
    End Property

    Public ReadOnly Property n_of_rows()
        Get
            Return MX_.GetLength(0)
        End Get
    End Property

    Public Sub loadarray(ByVal value(,) As Double)
        ReDim MX_(value.GetLength(0) - 1, value.GetLength(1) - 1)
        Array.Copy(value, MX_, value.Length)

        U_calc()
        det_calc()
        inv_calc()
    End Sub

    Private Sub det_calc()
        Dim i, r As Short
        r = Math.Min(UMX_.GetLength(0), UMX_.GetLength(1)) - 1
        det_ = 1
        For i = 0 To r Step 1
            det_ = det_ * UMX_(i, i)
        Next i
    End Sub

    Private Sub U_calc()
        Dim i, j, k, r As Short
        Dim p, q As Double
        ReDim UMX_(MX_.GetLength(0) - 1, MX_.GetLength(1) - 1)
        Array.Copy(MX_, UMX_, MX_.Length)
        r = Math.Min(MX_.GetLength(0), MX_.GetLength(1)) - 1
        For i = 0 To r Step 1
            p = UMX_(i, i)
            For j = (i + 1) To (MX_.GetLength(0) - 1) Step 1
                q = UMX_(j, i)
                For k = i To (MX_.GetLength(1) - 1) Step 1
                    UMX_(j, k) = UMX_(j, k) - UMX_(i, k) * q / p
                Next k
            Next j
        Next i
    End Sub

    Private Sub inv_calc()
        Dim i, j, k, r As Short
        Dim p, q As Double
        If (MX_.GetLength(0) - MX_.GetLength(1) = 0) And Not (det_ = 0) Then


            r = MX_.GetLength(0) - 1
            ReDim IMX_(r, r)
            For i = 0 To r Step 1
                For j = 0 To r Step 1
                    IMX_(i, j) = 0
                    If (i = j) Then IMX_(i, j) = 1
                Next j
            Next i
            Dim tempmx(r, r) As Double
            Array.Copy(MX_, tempmx, MX_.Length)

            For i = 0 To r Step 1
                p = tempmx(i, i)
                For j = (i + 1) To r Step 1
                    q = tempmx(j, i)
                    For k = i To r Step 1
                        tempmx(j, k) = tempmx(j, k) - tempmx(i, k) * q / p
                    Next k
                    For k = 0 To r Step 1
                        IMX_(j, k) = IMX_(j, k) - IMX_(i, k) * q / p
                    Next k
                Next j
            Next i

            For j = 0 To r Step 1
                p = tempmx(j, j)
                For i = 0 To r Step 1
                    tempmx(j, i) = tempmx(j, i) / p
                    IMX_(j, i) = IMX_(j, i) / p
                Next i
            Next j

            For i = r To 1 Step -1
                For j = 0 To (i - 1) Step 1
                    q = tempmx(j, i)
                    tempmx(j, i) = 0
                    For k = 0 To r
                        IMX_(j, k) = IMX_(j, k) - IMX_(i, k) * q
                    Next k
                Next j
            Next i


        Else
            IMX_ = Nothing
            Exit Sub
        End If
    End Sub

    Public Function ext_inv_calc(ByVal tempmatrix(,) As Double)
        Dim i, j, k, r As Short
        Dim p, q As Double
        r = tempmatrix.GetLength(0) - 1
        Dim ext_IMX_(r, r) As Double

        If (tempmatrix.GetLength(0) - tempmatrix.GetLength(1) = 0) Then
            For i = 0 To r Step 1
                For j = 0 To r Step 1
                    ext_IMX_(i, j) = 0
                    If (i = j) Then ext_IMX_(i, j) = 1
                Next j
            Next i
            Dim tempmx(r, r) As Double
            Array.Copy(tempmatrix, tempmx, tempmatrix.Length)

            For i = 0 To r Step 1
                p = tempmx(i, i)
                For j = (i + 1) To r Step 1
                    q = tempmx(j, i)
                    For k = i To r Step 1
                        tempmx(j, k) = tempmx(j, k) - tempmx(i, k) * q / p
                    Next k
                    For k = 0 To r Step 1
                        ext_IMX_(j, k) = ext_IMX_(j, k) - ext_IMX_(i, k) * q / p
                    Next k
                Next j
            Next i

            For j = 0 To r Step 1
                p = tempmx(j, j)
                For i = 0 To r Step 1
                    tempmx(j, i) = tempmx(j, i) / p
                    ext_IMX_(j, i) = ext_IMX_(j, i) / p
                Next i
            Next j

            For i = r To 1 Step -1
                For j = 0 To (i - 1) Step 1
                    q = tempmx(j, i)
                    tempmx(j, i) = 0
                    For k = 0 To r
                        ext_IMX_(j, k) = ext_IMX_(j, k) - ext_IMX_(i, k) * q
                    Next k
                Next j
            Next i


        Else
            Return Nothing
            Exit Function
        End If

        Return ext_IMX_
    End Function

    Public Function dotproduct(ByVal first(,) As Double, ByVal second(,) As Double)
        Dim x1, y1, x2, y2 As Short
        x1 = first.GetLength(0) - 1
        y1 = first.GetLength(1) - 1
        x2 = second.GetLength(0) - 1
        y2 = second.GetLength(1) - 1
        Dim i, j, k As Short
        Dim third(x1, y2) As Double

        If (y1 = x2) Then
            For i = 0 To x1 Step 1
                For j = 0 To y2 Step 1
                    third(i, j) = 0
                    For k = 0 To y1 Step 1
                        third(i, j) = third(i, j) + first(i, k) * second(k, j)
                    Next k
                Next j
            Next i
        Else
            third = Nothing
        End If
        Return third
    End Function

    Public Function adding(ByVal first(,) As Double, ByVal second(,) As Double)
        Dim x1, y1, x2, y2 As Short
        x1 = first.GetLength(0) - 1
        y1 = first.GetLength(1) - 1
        x2 = second.GetLength(0) - 1
        y2 = second.GetLength(1) - 1
        Dim i, j As Short
        Dim third(x1, y1) As Double
        If (x1 = x2) And (y1 = y2) Then
            For i = 0 To x1 Step 1
                For j = 0 To y1 Step 1
                    third(i, j) = first(i, j) + second(i, j)
                Next j
            Next i
        Else
            third = Nothing
        End If
        Return third
    End Function

    Public Function identity(ByVal n As Short)
        Dim IdMX(n - 1, n - 1) As Double
        Dim i, j As Short
        For i = 0 To n - 1 Step 1
            For j = 0 To n - 1 Step 1
                IdMX(i, j) = 0
                If (i = j) Then IdMX(i, j) = 1
            Next j
        Next i
        Return IdMX
    End Function

    Public Function dotscalar(ByVal c As Double, ByVal loadedmx(,) As Double)
        Dim i, j As Short
        Dim tempmx(loadedmx.GetLength(0) - 1, loadedmx.GetLength(1) - 1) As Double
        For i = 0 To tempmx.GetLength(0) - 1 Step 1
            For j = 0 To tempmx.GetLength(1) - 1 Step 1
                tempmx(i, j) = c * loadedmx(i, j)
            Next j
        Next i
        Return tempmx
    End Function

    Public Function transpose(ByVal loadedmx(,) As Double)
        Dim i, j As Short
        Dim tempmx(loadedmx.GetLength(1) - 1, loadedmx.GetLength(0) - 1) As Double
        For i = 0 To tempmx.GetLength(1) - 1 Step 1
            For j = 0 To tempmx.GetLength(0) - 1 Step 1
                tempmx(j, i) = loadedmx(i, j)
            Next j
        Next i
        Return tempmx
    End Function
End Class

Public Structure curve
    Dim time_() As Double
    Dim signal_() As Double
    Dim curve_(,) As Double
    Dim mintime_, maxtime_, minsignal_, maxsignal_, timeofminsignal_, timeofmaxsignal_ As Double

    Public Sub loadcurve(ByVal arrayvalue(,) As Double)
        ReDim curve_(arrayvalue.GetLength(0) - 1, arrayvalue.GetLength(1) - 1)
        Array.Copy(arrayvalue, curve_, arrayvalue.Length)

        ReDim time_(arrayvalue.GetLength(0) - 1)
        ReDim signal_(arrayvalue.GetLength(0) - 1)
        Dim i As Short
        For i = 0 To (arrayvalue.GetLength(0) - 1) Step 1
            time_(i) = arrayvalue(i, 0)
            signal_(i) = arrayvalue(i, 1)
        Next i

        minsignal_ = signal_(0)
        maxsignal_ = signal_(0)
        mintime_ = time_(0)
        maxtime_ = time_(0)
        For i = 0 To (arrayvalue.GetLength(0) - 1) Step 1
            If minsignal_ > signal_(i) Then
                minsignal_ = signal_(i)
                timeofminsignal_ = time_(i)
            ElseIf maxsignal_ < signal_(i) Then
                maxsignal_ = signal_(i)
                timeofmaxsignal_ = time_(i)
            End If
            mintime_ = Math.Min(mintime_, time_(i))
            maxtime_ = Math.Max(maxtime_, time_(i))
        Next i

    End Sub

    Public ReadOnly Property getcurve()
        Get
            Return curve_
        End Get
    End Property

    Public ReadOnly Property gettimes()
        Get
            Return time_
        End Get
    End Property

    Public ReadOnly Property getsignals()
        Get
            Return signal_
        End Get
    End Property

    Public ReadOnly Property getminsignal()
        Get
            Return minsignal_
        End Get
    End Property

    Public ReadOnly Property getmaxsignal()
        Get
            Return maxsignal_
        End Get
    End Property

    Public ReadOnly Property getmintime()
        Get
            Return mintime_
        End Get
    End Property

    Public ReadOnly Property getmaxtime()
        Get
            Return maxtime_
        End Get
    End Property

    Public ReadOnly Property gettimeofminsignal()
        Get
            Return timeofminsignal_
        End Get
    End Property

    Public ReadOnly Property gettimeofmaxsignal()
        Get
            Return timeofmaxsignal_
        End Get
    End Property

    Public ReadOnly Property getnofpoints()
        Get
            Return time_.Length
        End Get
    End Property
End Structure