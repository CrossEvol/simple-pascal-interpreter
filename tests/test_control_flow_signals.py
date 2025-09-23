"""Tests for control flow signal classes"""

import unittest
from spi.error import BreakSignal, ContinueSignal


class TestBreakSignal(unittest.TestCase):
    """Test cases for BreakSignal exception class"""

    def test_break_signal_creation(self):
        """Test that BreakSignal can be created and raised"""
        signal = BreakSignal()
        
        self.assertIsInstance(signal, BreakSignal)
        self.assertIsInstance(signal, Exception)
        self.assertEqual(str(signal), "Break from loop")

    def test_break_signal_can_be_raised_and_caught(self):
        """Test that BreakSignal can be raised and caught properly"""
        with self.assertRaises(BreakSignal) as context:
            raise BreakSignal()
        
        self.assertEqual(str(context.exception), "Break from loop")

    def test_break_signal_inheritance(self):
        """Test that BreakSignal inherits from Exception, not Error"""
        signal = BreakSignal()
        
        # Should inherit from Exception
        self.assertIsInstance(signal, Exception)
        
        # Should NOT inherit from Error (to avoid being treated as error)
        from spi.error import Error
        self.assertNotIsInstance(signal, Error)


class TestContinueSignal(unittest.TestCase):
    """Test cases for ContinueSignal exception class"""

    def test_continue_signal_creation(self):
        """Test that ContinueSignal can be created and raised"""
        signal = ContinueSignal()
        
        self.assertIsInstance(signal, ContinueSignal)
        self.assertIsInstance(signal, Exception)
        self.assertEqual(str(signal), "Continue to next loop iteration")

    def test_continue_signal_can_be_raised_and_caught(self):
        """Test that ContinueSignal can be raised and caught properly"""
        with self.assertRaises(ContinueSignal) as context:
            raise ContinueSignal()
        
        self.assertEqual(str(context.exception), "Continue to next loop iteration")

    def test_continue_signal_inheritance(self):
        """Test that ContinueSignal inherits from Exception, not Error"""
        signal = ContinueSignal()
        
        # Should inherit from Exception
        self.assertIsInstance(signal, Exception)
        
        # Should NOT inherit from Error (to avoid being treated as error)
        from spi.error import Error
        self.assertNotIsInstance(signal, Error)


if __name__ == "__main__":
    unittest.main()