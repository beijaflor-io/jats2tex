resource "aws_iam_user" "edo" {
  name = "edo"
}

resource "aws_iam_access_key" "edo" {
  user = "${aws_iam_user.edo.name}"
}

resource "aws_iam_user_policy" "edo_ses" {
  name = "Send e-mails for EDO"
  user = "${aws_iam_user.edo.name}"
  policy = <<EOF
{
  "Version": "2012-1017",
  "Statement": [
    {
      "Action": [
        "ses:SendEmail"
      ],
      "Effect": "Allow",
      "Resource": "*"
    }
  ]
}
EOF
}
