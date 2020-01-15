Public Class Form1
    Dim MX1 As New Matrix

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Dim x, y As Short
        x = CShort(TextBox1.Text) - 1
        y = CShort(TextBox2.Text) - 1
        Dim a(x, y) As Single
        Dim b(y, x) As Single
        RichTextBox1.Clear()

        Array.Copy(generatearray(x, y), a, a.Length)
        printmx(a)
        printmx(MX1.dotproduct(a, MX1.transpose(a)))
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

    Private Sub printmx(ByVal mx(,) As Single)
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

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        Me.Close()
    End Sub

    Public Function generatearray(ByVal x_col As Short, ByVal y_row As Short)
        Dim i, j As Short
        Dim MX(x_col, y_row) As Single
        For i = 0 To x_col Step 1
            For j = 0 To y_row Step 1
                MX(i, j) = Rnd()
            Next j
        Next i
        Return MX
    End Function
End Class

Public Class Matrix
    Private MX_(,) As Single
    Private UMX_(,) As Single
    Private IMX_(,) As Single
    Private det_ As Single

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
        Dim p, q As Single
        ReDim UMX_(MX_.GetLength(0) - 1, MX_.GetLength(1) - 1)
        Array.Copy(MX_, UMX_, MX_.Length)
        r = Math.Min(MX_.GetLength(0), MX_.GetLength(1)) - 1
        For i = 0 To r Step 1
            p = UMX_(i, i)
            For j = (i + 1) To (MX_.GetLength(1) - 1) Step 1
                q = UMX_(j, i)
                For k = i To (MX_.GetLength(0) - 1) Step 1
                    UMX_(j, k) = UMX_(j, k) - UMX_(i, k) * q / p
                Next k
            Next j
        Next i
    End Sub

    Private Sub inv_calc()
        Dim i, j, k, r As Short
        Dim p, q As Single
        If (MX_.GetLength(0) - MX_.GetLength(1) = 0) And Not (det_ = 0) Then


            r = MX_.GetLength(0) - 1
            ReDim IMX_(r, r)
            For i = 0 To r Step 1
                For j = 0 To r Step 1
                    IMX_(i, j) = 0
                    If (i = j) Then IMX_(i, j) = 1
                Next j
            Next i
            Dim tempmx(r, r) As Single
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

    Public ReadOnly Property Upper()
        Get
            Return UMX_
        End Get
    End Property

    Public ReadOnly Property Invert()
        Get
            Return IMX_
        End Get
    End Property

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

    Public Sub loadarray(ByVal value(,) As Single)
        ReDim MX_(value.GetLength(0) - 1, value.GetLength(1) - 1)
        Array.Copy(value, MX_, value.Length)

        U_calc()
        det_calc()
        inv_calc()
    End Sub

    Public Function dotproduct(ByVal first(,) As Single, ByVal second(,) As Single)
        Dim x1, y1, x2, y2 As Short
        x1 = first.GetLength(0) - 1
        y1 = first.GetLength(1) - 1
        x2 = second.GetLength(0) - 1
        y2 = second.GetLength(1) - 1
        Dim i, j, k As Short
        Dim third(x1, y2) As Single

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

    Public Function adding(ByVal first(,) As Single, ByVal second(,) As Single)
        Dim x1, y1, x2, y2 As Short
        x1 = first.GetLength(0) - 1
        y1 = first.GetLength(1) - 1
        x2 = second.GetLength(0) - 1
        y2 = second.GetLength(1) - 1
        Dim i, j As Short
        Dim third(x1, y1) As Single
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
        Dim IdMX(n - 1, n - 1) As Single
        Dim i, j As Short
        For i = 0 To n - 1 Step 1
            For j = 0 To n - 1 Step 1
                IdMX(i, j) = 0
                If (i = j) Then IdMX(i, j) = 1
            Next j
        Next i
        Return IdMX
    End Function

    Public Function dotscalar(ByVal c As Single, ByVal loadedmx(,) As Single)
        Dim i, j As Short
        Dim tempmx(loadedmx.GetLength(0) - 1, loadedmx.GetLength(1) - 1) As Single
        For i = 0 To tempmx.GetLength(0) - 1 Step 1
            For j = 0 To tempmx.GetLength(1) - 1 Step 1
                tempmx(i, j) = c * loadedmx(i, j)
            Next j
        Next i
        Return tempmx
    End Function

    Public Function transpose(ByVal loadedmx(,) As Single)
        Dim i, j As Short
        Dim tempmx(loadedmx.GetLength(1) - 1, loadedmx.GetLength(0) - 1) As Single
        For i = 0 To tempmx.GetLength(1) - 1 Step 1
            For j = 0 To tempmx.GetLength(0) - 1 Step 1
                tempmx(j, i) = loadedmx(i, j)
            Next j
        Next i
        Return tempmx
    End Function
End Class
