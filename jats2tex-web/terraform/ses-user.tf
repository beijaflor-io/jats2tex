resource "aws_iam_user" "edo" {
  name = "edo"
}

resource "aws_iam_access_key" "edo" {
  user = "${aws_iam_user.edo.name}"
}

resource "aws_iam_user_policy" "edo_ses" {
  name = "ses_send_email"
  user = "${aws_iam_user.edo.name}"
  policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "ses:SendEmail",
        "ses:SendRawEmail"
      ],
      "Effect": "Allow",
      "Resource": "*"
    }
  ]
}
EOF
}
